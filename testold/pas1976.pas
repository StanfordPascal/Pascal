

(*******************)
(*$C+,D+,L+,X+     *)
(*******************)


program PASCALCOMPILER ( INPUT , OUTPUT , PRR ) ;

(**********************************************)
(*                                            *)
(*                                            *)
(*         PORTABLE PASCAL COMPILER           *)
(*         ************************           *)
(*                                            *)
(*                PASCAL P4                   *)
(*                                            *)
(*                                            *)
(*     AUTHORS:                               *)
(*              URS AMMANN                    *)
(*              KESAV NORI                    *)
(*              CHRISTIAN JACOBI              *)
(*                                            *)
(*     ADDRESS:                               *)
(*                                            *)
(*          INSTITUT FUER INFORMATIK          *)
(*          EIDG. TECHNISCHE HOCHSCHULE       *)
(*          CH-8096 ZUERICH                   *)
(*                                            *)
(*                                            *)
(*     LAST CHANGES COMPLETED IN MAY 1976     *)
(*                                            *)
(**********************************************)
(*                                            *)
(*     ADAPTED AND MODIFIED BY                *)
(*                                            *)
(*     - JUERGEN REICHMANN   1985             *)
(*     - BERND OPPOLZER      2019             *)
(*                                            *)
(**********************************************)



const MAXERRS = 30 ;
      DISPLIMIT = 20 ;
      MAXLEVEL = 10 ;
      WORDAL = 2 ;
      INTSIZE = 4 ;
      INTAL = WORDAL ;
      REALSIZE = 4 ;
      REALAL = WORDAL ;
      CHARSIZE = 1 ;
      CHARAL = 1 ;
      CHARMAX = 1 ;
      BOOLSIZE = 1 ;
      BOOLAL = 1 ;
      PTRSIZE = INTSIZE ;
      ADRAL = INTAL ;
      SETSIZE = 32 ;
      SETAL = WORDAL ;
      STACKAL = WORDAL ;
      STACKELSIZE = 1 ;
      ALPHALEN = 16 ;
      SUCCALPHALEN = 17 ;
      STRGLGTH = 80 ;
      RSTRGLGTH = 12 ;
      MAXLINELEN = 128 ;
      SETHIGH = 255 ;
      SETLOW = 0 ;
      ORDMAXCHAR = 255 ;
      ORDMINCHAR = 0 ;
      TAB = 9 ;
      MAXINT = 2147483647 ;
      LCBEFOREMARKSTACK = 8 ;
      LCAFTERMARKSTACK = 0 ;
      FILEAL = ADRAL ;
      FCBSIZE = 16 ;
      MAXSTACK = 1 ;
      PARMAL = WORDAL ;
      PARMSIZE = PTRSIZE ;
      RECAL = STACKAL ;
      FILEBUFFER = 2 ;
      MAXADDR = 16777215 ;


type SYMBOL = ( IDENT , INTCONST , REALCONST , STRINGCONST , NOTSY ,
              MULOP , ADDOP , RELOP , LPARENT , RPARENT , LBRACK ,
              RBRACK , COMMA , SEMICOLON , PERIOD , ARROW , COLON ,
              BECOMES , LABELSY , CONSTSY , TYPESY , VARSY , FUNCSY ,
              PROGSY , PROCSY , SETSY , PACKEDSY , ARRAYSY , RECORDSY ,
              FILESY , FORWARDSY , BEGINSY , IFSY , CASESY , REPEATSY ,
              WHILESY , FORSY , WITHSY , GOTOSY , ENDSY , ELSESY ,
              UNTILSY , OFSY , DOSY , TOSY , DOWNTOSY , THENSY ,
              IMPORTSY , EXPORTSY , STRINGSY , OTHERWSY , OTHERSY ) ;
     OPERATOR = ( MUL , RDIV , ANDOP , IDIV , IMOD , PLUS , MINUS ,
                OROP , LTOP , LEOP , GEOP , GTOP , NEOP , EQOP , INOP ,
                NOOP ) ;
     SETOFSYS = set of SYMBOL ;
     CHTP = ( LETTER , NUMBER , SPECIAL , ILLEGAL ) ;
     CSTCLASS = ( REEL , PSET , STRG ) ;
     CSP = -> CONSTANT ;
     CONSTANT = record
                  case CCLASS : CSTCLASS of
                    REEL :
                      ( RVAL : packed array [ 1 .. RSTRGLGTH ] of CHAR
                        ) ;
                    PSET :
                      ( PVAL : set of SETLOW .. SETHIGH ) ;
                    STRG :
                      ( SLGTH : 0 .. STRGLGTH ;
                        SVAL : packed array [ 1 .. STRGLGTH ] of CHAR )
                end ;
     VALU = record
              case INTVAL : BOOLEAN of
                TRUE :
                  ( IVAL : INTEGER ) ;
                FALSE :
                  ( VALP : CSP )
            end ;
     LEVRANGE = 0 .. MAXLEVEL ;
     ADDRRANGE = - MAXADDR .. MAXADDR ;
     STRUCTFORM = ( SCALAR , SUBRANGE , POINTER , POWER , ARRAYS ,
                  RECORDS , FILES , TAGFLD , VARIANT ) ;
     DECLKIND = ( STANDARD , DECLARED ) ;
     STP = -> STRUCTURE ;
     CTP = -> IDENTIFIER ;
     STRUCTURE = packed record
                          MARKED : BOOLEAN ;
                          SIZE : ADDRRANGE ;
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
                              ( AELTYPE , INXTYPE : STP ;
                                VARY : BOOLEAN ) ;
                            RECORDS :
                              ( FSTFLD : CTP ;
                                RECVAR : STP ) ;
                            FILES :
                              ( FILTYPE : STP ) ;
                            TAGFLD :
                              ( TAGFIELDP : STP ;
                                FSTVAR : STP ) ;
                            VARIANT :
                              ( NXTVAR , SUBVAR : STP ;
                                VARVAL : VALU )
                        end ;
     IDCLASS = ( TYPES , KONST , VARS , FIELD , PROC , FUNC ) ;
     SETOFIDS = set of IDCLASS ;
     IDKIND = ( ACTUAL , FORMAL ) ;
     PF_ATTRIB = ( INTERN , EXPRT , IMPRT ) ;
     ALPHA = packed array [ 1 .. ALPHALEN ] of CHAR ;
     IDENTIFIER = packed record
                           NAME : ALPHA ;
                           LLINK , RLINK : CTP ;
                           IDTYPE : STP ;
                           NEXT : CTP ;
                           case KLASS : IDCLASS of
                             KONST :
                               ( VALUES : VALU ) ;
                             VARS :
                               ( VKIND : IDKIND ;
                                 VLEV : LEVRANGE ;
                                 VADDR : ADDRRANGE ;
                                 VXNAME : CSP ;
                                 VIMPORT : BOOLEAN ) ;
                             FIELD :
                               ( FLDADDR : ADDRRANGE ) ;
                             PROC , FUNC :
                               ( case PFDECKIND : DECLKIND of
                                   STANDARD :
                                     ( KEY : 1 .. 15 ) ;
                                   DECLARED :
                                     ( PFLEV : LEVRANGE ;
                                       PFNAME , NUMB_OF_PARM ,
                                       RESULT_LABEL : INTEGER ;
                                       case PFKIND : IDKIND of
                                         ACTUAL :
                                           ( FORWDECL : BOOLEAN ;
                                             PFXNAME : CSP ;
                                             PF_ATTR : PF_ATTRIB ) ) )
                         end ;
     DISPRANGE = 0 .. DISPLIMIT ;
     WHERE = ( BLCK , CREC , VREC , REC ) ;
     ATTRKIND = ( CST , VARBL , EXPR ) ;
     VACCESS = ( DRCT , INDRCT , INXD ) ;
     ATTR = record
              TYPTR : STP ;
              LABEL_ACCESS : BOOLEAN ;
              case KIND : ATTRKIND of
                CST :
                  ( CVAL : VALU ) ;
                VARBL :
                  ( ACP : CTP ;
                    case ACCESS : VACCESS of
                      DRCT :
                        ( VLEVEL : LEVRANGE ;
                          DPLMT : ADDRRANGE ) ;
                      INDRCT :
                        ( IDPLMT : ADDRRANGE ) )
            end ;
     TESTP = -> TESTPOINTER ;
     TESTPOINTER = packed record
                            ELT1 , ELT2 : STP ;
                            LASTTESTP : TESTP ;
                            COMPAT : BOOLEAN ;
                          end ;
     LBP = -> LABL ;
     LABL = record
              NEXTLAB : LBP ;
              DEFINED : BOOLEAN ;
              LABVAL , LABNAME : INTEGER
            end ;
     EXTFILEP = -> FILEREC ;
     FILEREC = record
                 FILENAME : ALPHA ;
                 NEXTFILE : EXTFILEP
               end ;


var SY : SYMBOL ;
    OP : OPERATOR ;
    VAL : VALU ;
    LGTH : INTEGER ;
    ID : ALPHA ;
    KK : 1 .. ALPHALEN ;
    CH : CHAR ;
    EOL : BOOLEAN ;
    CHCNT : INTEGER ;
    LC , IC : ADDRRANGE ;
    OLDLINECOUNT , LINECOUNT : INTEGER ;
    EXPECTSTRING , DP , PRTERR , LIST , PRCODE : BOOLEAN ;
    DEBUG : BOOLEAN ;
    PARMPTR , INTPTR , REALPTR , CHARPTR , BOOLPTR , NILPTR , TEXTPTR :
                                                   STP ;
    UTYPPTR , UCSTPTR , UVARPTR , UFLDPTR , UPRCPTR , UFCTPTR , FWPTR :
                                                   CTP ;
    FEXTFILEP : EXTFILEP ;
    FILECP : CTP ;
    GLOBTESTP : TESTP ;
    MIN_LEVEL , LEVEL : LEVRANGE ;
    STNEST , LASTSTNEST : INTEGER ;
    DISX , TOP : DISPRANGE ;
    PRR : TEXT ;
    DISPLAY : array [ DISPRANGE ] of packed record
                                              FNAME : CTP ;
                                              FLABEL : LBP ;
                                              case OCCUR : WHERE of
                                                CREC :
                                                  ( CLEV : LEVRANGE ;
                                                   CDSPL : ADDRRANGE )
                                                   ;
                                                VREC :
                                                  ( VDSPL : ADDRRANGE )
                                            end ;
    ERRINX : 0 .. 10 ;
    TOTALERRS : INTEGER ;
    ERRLIST : array [ 1 .. 10 ] of packed record
                                            POS : INTEGER ;
                                            NMR : 1 .. 400
                                          end ;
    GATTR : ATTR ;
    CONSTBEGSYS , SIMPTYPEBEGSYS , TYPEBEGSYS , BLOCKBEGSYS , SELECTSYS
    , FACBEGSYS , STATBEGSYS , TYPEDELS : SETOFSYS ;
    CHARTP : array [ CHAR ] of CHTP ;
    RW : array [ 1 .. 39 ] of ALPHA ;
    FRW : array [ 1 .. SUCCALPHALEN ] of 1 .. 40 ;
    RSY : array [ 1 .. 39 ] of SYMBOL ;
    SSY : array [ CHAR ] of SYMBOL ;
    ROP : array [ 1 .. 39 ] of OPERATOR ;
    SOP : array [ CHAR ] of OPERATOR ;
    NA : array [ 1 .. 37 ] of ALPHA ;
    MN : array [ 0 .. 67 ] of packed array [ 1 .. 4 ] of CHAR ;
    SNA : array [ 1 .. 25 ] of packed array [ 1 .. 4 ] of CHAR ;
    CDX : array [ 0 .. 67 ] of - 4 .. + 4 ;
    PDX : array [ 1 .. 25 ] of - 7 .. + 7 ;
    ORDINT : array [ CHAR ] of INTEGER ;
    INTLABEL , MXINT10 , DIGMAX : INTEGER ;
    CRNTLNPOS : INTEGER ;
    CRNTLN : array [ 1 .. MAXLINELEN ] of CHAR ;
    FNAME : packed array [ 0 .. STRGLGTH ] of CHAR ;
    FNAMELEN : INTEGER ;



procedure WRITEFNAME ;

   var I : INTEGER ;

   begin (* WRITEFNAME *)
     WRITE ( '"' ) ;
     for I := 0 to FNAMELEN - 1 do
       WRITE ( FNAME [ I ] ) ;
     WRITE ( '"' ) ;
   end (* WRITEFNAME *) ;



procedure PRINTERR ( ERR : INTEGER ) ;

   var S : STRING ( 80 ) ;

   begin (* PRINTERR *)
     case ERR of
       1 : S := 'error in simple type' ;
       2 : S := 'identifier expected' ;
       4 : S := ''')'' expected' ;
       5 : S := ''':'' expected' ;
       6 : S := 'illegal symbol' ;
       7 : S := 'error in parameter list' ;
       8 : S := '''of'' expected' ;
       9 : S := '''('' expected' ;
       10 : S := 'error in type' ;
       11 : S := '''['' expected' ;
       12 : S := ''']'' expected' ;
       13 : S := '''end'' expected' ;
       14 : S := ''';'' expected' ;
       15 : S := 'integer expected' ;
       16 : S := '''='' expected' ;
       17 : S := '''begin'' expected' ;
       18 : S := 'error in declaration part' ;
       19 : S := 'error in field list' ;
       20 : S := ''','' expected' ;
       21 : S := '''*'' expected' ;
       22 : S := 'constant expected' ;
       23 : S := 'string constant expected' ;
       50 : S := 'error in constant' ;
       51 : S := ''':='' expected' ;
       52 : S := '''then'' expected' ;
       53 : S := '''until'' expected' ;
       54 : S := '''do'' expected' ;
       55 : S := '''to'' or ''downto'' expected' ;
       58 : S := 'error in factor' ;
       59 : S := 'error in variable' ;
       101 : S := 'identifier declared twice' ;
       102 : S := 'low bound exceeds high bound' ;
       103 : S := 'identifier is not of appropriate class' ;
       104 : S := 'identifier not declared' ;
       105 : S := 'sign not allowed' ;
       106 : S := 'number expected' ;
       107 : S := 'incompatible subrange types' ;
       108 : S := 'file not allowed here' ;
       109 : S := 'type must not be real' ;
       110 : S := 'tagfield type must be scalar or subrange' ;
       111 : S := 'incompatible with tagfield type' ;
       113 : S := 'index type must be scalar or subrange' ;
       114 : S := 'base type must not be real' ;
       115 : S := 'base type must be scalar or subrange' ;
       116 : S := 'error in type of standard procedure parameter' ;
       117 : S := 'unsatisfied forward reference' ;
       119 : S :=
           'forward declared: repetition of parameterlist not allowed'
                  ;
       120 : S := 'function result must be scalar, subrange or pointer'
                  ;
       121 : S := 'file value parameter not allowed' ;
       122 : S :=
    'forward declared: repetition of function result type not allowed'
                  ;
       123 : S := 'missing function result type' ;
       124 : S := 'F-format for real only' ;
       125 : S := 'error in type of standard function parameter' ;
       126 : S :=
                'number of parameters does not agree with declaration'
                  ;
       128 : S :=
   'result type of parameter function does not agree with declaration'
                  ;
       129 : S := 'type conflict of operands' ;
       130 : S := 'expression is not of set type' ;
       131 : S := 'tests on equality allowed only' ;
       132 : S := 'strict inclusion not allowed' ;
       133 : S := 'file comparison not allowed' ;
       134 : S := 'illegal type of operand(s)' ;
       135 : S := 'type of operand must be boolean' ;
       136 : S := 'set element type must be scalar or subrange' ;
       137 : S := 'set element types not compatible' ;
       138 : S := 'type of variable is not array or string' ;
       139 : S := 'index type is not compatible with declaration' ;
       140 : S := 'type of variable is not record' ;
       141 : S := 'type of variable must be file or pointer' ;
       142 : S := 'illegal parameter substitution' ;
       143 : S := 'illegal type of loop control variable' ;
       144 : S := 'illegal type of expression' ;
       145 : S := 'type conflict' ;
       146 : S := 'assignment of files not allowed' ;
       147 : S := 'label type incompatible with selecting expression' ;
       148 : S := 'subrange bounds must be scalar' ;
       149 : S := 'index type must not be integer' ;
       150 : S := 'assignment to standard function is not allowed' ;
       151 : S := 'assignment to formal function is not allowed' ;
       152 : S := 'no such field in this record' ;
       154 : S := 'actual parameter must be a variable' ;
       155 : S := 'control variable must not be a variable parameter' ;
       156 : S := 'multidefined case label' ;
       157 : S := 'too many cases in this statement' ;
       158 : S := 'missing corresponding variant declration' ;
       159 : S := 'real or string tagfields not allowed' ;
       160 : S := 'previous declaration was not forward' ;
       161 : S := 'again forward declared' ;
       162 : S := 'parameter size must be constant' ;
       165 : S := 'multidefined label' ;
       166 : S := 'multideclared label' ;
       167 : S := 'undeclared label' ;
       168 : S := 'undefined label' ;
       177 : S := 'assignment to function identifier not allowed here'
                  ;
       178 : S := 'multidefined record variant' ;
       201 : S := 'error in real constant: digit expected' ;
       202 : S :=
              'string constant must not exceed source line without \\'
                  ;
       203 : S := 'integer constant exceeds range' ;
       250 : S := 'too many nested scopes of identifiers' ;
       251 : S := 'too many nested procedures and/or functions' ;
       254 : S := 'too many long constants in this procedure' ;
       304 : S := 'element expression out of range' ;
       399 : S := 'implementation restriction' ;
       400 , 401 , 402 :
         S := 'compiler error' ;
       otherwise
         S := 'sorry, no text' ;
     end (* case *) ;
     WRITEFNAME ;
     WRITELN ( ': ' , ERR : 3 , ' ' , S ) ;
   end (* PRINTERR *) ;



procedure ENDOFLINE ;

   var LASTPOS , FREEPOS , CURRPOS , CURRNMR , F , K : INTEGER ;

   begin (* ENDOFLINE *)
     if LIST or ( ERRINX > 0 ) then
       begin
         WRITE ( OUTPUT , LINECOUNT : 4 , ' ' : 1 ) ;
         if DP then
           WRITE ( OUTPUT , LC : 5 )
         else
           WRITE ( OUTPUT , IC : 5 ) ;
         WRITE ( OUTPUT , ' ' , LEVEL : 1 , ' ' , LASTSTNEST : 2 , ' '
                 ) ;
         for K := 1 to CRNTLNPOS do
           WRITE ( OUTPUT , CRNTLN [ K ] : 1 ) ;
         WRITELN ( OUTPUT ) ;
       end (* then *) ;
     if ERRINX > 0 then
       begin
         TOTALERRS := TOTALERRS + ERRINX ;
         WRITE ( OUTPUT , '****' , ' ' : 11 ) ;
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
         for K := 1 to ERRINX do
           PRINTERR ( ERRLIST [ K ] . NMR ) ;
         ERRINX := 0 ;
         if TOTALERRS > MAXERRS then
           begin
             WRITELN ( '****  Too many errors found, good bye !' ) ;
             EXIT ( - 1 ) ;
           end (* then *) ;
       end (* then *) ;
     LINECOUNT := SUCC ( LINECOUNT ) ;
     CHCNT := 0 ;
     CRNTLNPOS := 0 ;
     LASTSTNEST := STNEST ;
   end (* ENDOFLINE *) ;



procedure ERROR ( FERRNR : INTEGER ) ;

   begin (* ERROR *)
     PRCODE := FALSE ;
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
     ERRLIST [ ERRINX ] . POS := CHCNT
   end (* ERROR *) ;



procedure INSYMBOL ;

   label 1 , 2 , 3 , 4 , 5 ;

   var I , K : INTEGER ;
       DIGIT : packed array [ 1 .. RSTRGLGTH ] of CHAR ;
       STRNG : packed array [ 1 .. STRGLGTH ] of CHAR ;
       LVP : CSP ;
       SKIP , TEST : BOOLEAN ;


   procedure NEXTCHLOWER ;

      label 1 ;

      var I : INTEGER ;

      begin (* NEXTCHLOWER *)
        if EOL then
          ENDOFLINE ;
        1 :
        if not EOF ( INPUT ) then
          begin
            EOL := EOLN ( INPUT ) ;
            READ ( INPUT , CH ) ;
            if CRNTLNPOS < MAXLINELEN then
              begin
                if ( CRNTLNPOS = 0 ) and ( CH = '#' ) then
                  begin
                    READ ( INPUT , CH ) ;
                    READ ( INPUT , CH ) ;
                    LINECOUNT := 0 ;
                    while CHARTP [ CH ] = NUMBER do
                      begin
                        LINECOUNT := LINECOUNT * 10 + ORDINT [ CH ] ;
                        EOL := EOLN ( INPUT ) ;
                        READ ( INPUT , CH ) ;
                      end (* while *) ;
                    while not EOL and ( CH <> '"' ) do
                      begin
                        EOL := EOLN ( INPUT ) ;
                        READ ( INPUT , CH ) ;
                      end (* while *) ;
                    if not EOL then
                      begin
                        I := 0 ;
                        repeat
                          EOL := EOLN ( INPUT ) ;
                          READ ( INPUT , CH ) ;
                          TEST := EOL or ( CH = '"' ) or ( I > STRGLGTH
                                  ) ;
                          if not TEST then
                            begin
                              FNAME [ I ] := CH ;
                              I := SUCC ( I ) ;
                            end (* then *) ;
                        until TEST ;
                        FNAMELEN := I ;
                        while not EOL do
                          begin
                            EOL := EOLN ( INPUT ) ;
                            READ ( INPUT , CH ) ;
                          end (* while *) ;
                        if LIST then
                          begin
                            WRITELN ;
                            WRITE ( 'file ' ) ;
                            WRITEFNAME ;
                            WRITELN ;
                            WRITELN ;
                          end (* then *) ;
                      end (* then *) ;
                    goto 1 ;
                  end (* then *) ;
                CRNTLNPOS := SUCC ( CRNTLNPOS ) ;
                CRNTLN [ CRNTLNPOS ] := CH ;
              end (* then *) ;
            if CH = CHR ( TAB ) then
              begin
                CH := ' ' ;
                CHCNT := ( CHCNT + 8 ) DIV 8 * 8 ;
              end (* then *)
            else
              CHCNT := SUCC ( CHCNT )
          end (* then *)
        else
          begin
            WRITELN ( OUTPUT , '****  End of file not expected here' )
                      ;
            EXIT ( - 1 ) ;
          end (* else *)
      end (* NEXTCHLOWER *) ;


   procedure NEXTCH ;

      begin (* NEXTCH *)
        NEXTCHLOWER ;
        if ( CH >= 'a' ) and ( CH <= 'z' ) then
          CH := CHR ( ORD ( CH ) - ORD ( 'a' ) + ORD ( 'A' ) ) ;
      end (* NEXTCH *) ;


   procedure OPTIONS ;

      var TMP : BOOLEAN ;

      begin (* OPTIONS *)
        repeat
          NEXTCH ;
          if ( CH <> '*' ) and ( CH <> '}' ) then
            begin
              if CH = 'L' then
                begin
                  NEXTCH ;
                  LIST := CH = '+' ;
                end (* then *)
              else
                if CH = 'D' then
                  begin
                    NEXTCH ;
                    DEBUG := CH = '+'
                  end (* then *)
                else
                  if CH = 'C' then
                    begin
                      NEXTCH ;
                      PRCODE := CH = '+'
                    end (* then *)
                  else
                    if CH = 'S' then
                      SKIP := TRUE
                    else
                      if CH = 'A' then
                        begin
                          WRITE ( PRR , '@' ) ;
                          repeat
                            NEXTCH ;
                            if EOL then
                              begin
                                NEXTCH ;
                                WRITELN ( PRR ) ;
                                WRITE ( PRR , '@' ) ;
                              end (* then *) ;
                            if CH <> '}' then
                              WRITE ( PRR , CH ) ;
                          until ( CH = '}' ) or EOF ;
                          WRITELN ( PRR ) ;
                          if EOF then
                            begin
                              WRITELN ;
                              WRITE ( '****  "}" expected' ) ;
                              WRITELN (
                             '****  (Assembly only between {$A and })'
                                        ) ;
                            end (* then *) ;
                        end (* then *) ;
              if CH <> '}' then
                NEXTCH
            end (* then *)
        until CH <> ','
      end (* OPTIONS *) ;


   begin (* INSYMBOL *)
     1 :
     repeat
       while ( CH = ' ' ) and not EOL do
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
       case CH of
         'A' , 'B' , 'C' , 'D' , 'E' , 'F' , 'G' , 'H' , 'I' , 'J' ,
         'K' , 'L' , 'M' , 'N' , 'O' , 'P' , 'Q' , 'R' , 'S' , 'T' ,
         'U' , 'V' , 'W' , 'X' , 'Y' , 'Z' , '_' :
           begin
             K := 0 ;
             repeat
               if K < ALPHALEN then
                 begin
                   K := K + 1 ;
                   ID [ K ] := CH
                 end (* then *) ;
               NEXTCH
             until CHARTP [ CH ] in [ SPECIAL , ILLEGAL ] ;
             if K >= KK then
               KK := K
             else
               repeat
                 ID [ KK ] := ' ' ;
                 KK := KK - 1
               until KK = K ;
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
         '0' , '1' , '2' , '3' , '4' , '5' , '6' , '7' , '8' , '9' ,
         '$' : begin
                 OP := NOOP ;
                 I := 0 ;
                 5 :
                 repeat
                   I := I + 1 ;
                   if I <= DIGMAX then
                     DIGIT [ I ] := CH ;
                   NEXTCH
                 until CHARTP [ CH ] <> NUMBER ;
                 if DIGIT [ 1 ] = '$' then
                   if ( CH >= 'A' ) and ( CH <= 'F' ) then
                     goto 5 ;
                 if ( CH = '.' ) or ( CH = 'E' ) then
                   begin
                     K := I ;
                     if CH = '.' then
                       begin
                         K := K + 1 ;
                         if K <= DIGMAX then
                           DIGIT [ K ] := CH ;
                         NEXTCH ;
                         if CH = '.' then
                           begin
                             CH := ':' ;
                             goto 3
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
                     if CH = 'E' then
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
                     if DIGIT [ 1 ] = '$' then
                       begin
                         ERROR ( 201 ) ;
                         DIGIT [ 1 ] := '0' ;
                       end (* then *) ;
                     NEW ( LVP , REEL ) ;
                     SY := REALCONST ;
                     LVP -> . CCLASS := REEL ;
                     with LVP -> do
                       begin
                         for I := 1 to RSTRGLGTH do
                           RVAL [ I ] := ' ' ;
                         if K <= DIGMAX then
                           for I := 2 to K + 1 do
                             RVAL [ I ] := DIGIT [ I - 1 ]
                         else
                           begin
                             ERROR ( 203 ) ;
                             RVAL [ 2 ] := '0' ;
                             RVAL [ 3 ] := '.' ;
                             RVAL [ 4 ] := '0'
                           end (* else *)
                       end (* with *) ;
                     VAL . VALP := LVP
                   end (* then *)
                 else
                   3 :
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
                           if DIGIT [ 1 ] = '$' then
                             for K := 2 to I do
                               begin
                                 if IVAL <= MXINT10 then
                                   begin
                                     IVAL := IVAL * 16 ;
                                     if DIGIT [ K ] < 'A' then
                                       IVAL := IVAL + ORDINT [ DIGIT [
                                               K ] ]
                                     else
                                       IVAL := IVAL + ORD ( DIGIT [ K ]
                                               ) - ORD ( 'A' ) + 10 ;
                                   end (* then *)
                                 else
                                   begin
                                     ERROR ( 203 ) ;
                                     IVAL := 0
                                   end (* else *)
                               end (* for *)
                           else
                             for K := 1 to I do
                               begin
                                 if IVAL <= MXINT10 then
                                   IVAL := IVAL * 10 + ORDINT [ DIGIT [
                                           K ] ]
                                 else
                                   begin
                                     ERROR ( 203 ) ;
                                     IVAL := 0
                                   end (* else *)
                               end (* for *) ;
                           SY := INTCONST
                         end (* with *)
                   end
               end (* tag/ca *) ;
         '''' : begin
                  LGTH := 0 ;
                  SY := STRINGCONST ;
                  OP := NOOP ;
                  repeat
                    repeat
                      4 :
                      NEXTCHLOWER ;
                      if CH = '\' then
                        begin
                          NEXTCHLOWER ;
                          case CH of
                            'n' : CH := CHR ( 10 ) ;
                            't' : CH := CHR ( 9 ) ;
                            'b' : CH := CHR ( 8 ) ;
                            'r' : CH := CHR ( 13 ) ;
                            'f' : CH := CHR ( 12 ) ;
                            '0' , '1' , '2' , '3' :
                              begin
                                I := 0 ;
                                K := 0 ;
                                while ( CH >= '0' ) and ( CH <= '7' )
                                and ( K < 3 ) do
                                  begin
                                    I := I * 8 ;
                                    I := I + ORD ( CH ) - ORD ( '0' ) ;
                                    K := SUCC ( K ) ;
                                    if ( K < 3 ) then
                                      NEXTCHLOWER ;
                                  end (* while *) ;
                                CH := CHR ( I ) ;
                              end (* tag/ca *) ;
                            otherwise
                              
                            : if EOL then
                                goto 4 ;
                          end (* case *) ;
                        end (* then *) ;
                      LGTH := LGTH + 1 ;
                      if LGTH <= STRGLGTH then
                        STRNG [ LGTH ] := CH
                    until ( EOL ) or ( CH = '''' ) ;
                    if EOL then
                      ERROR ( 202 )
                    else
                      NEXTCH
                  until CH <> '''' ;
                  LGTH := LGTH - 1 ;
                  if not EXPECTSTRING and ( LGTH = 1 ) then
                    VAL . IVAL := ORD ( STRNG [ 1 ] )
                  else
                    begin
                      NEW ( LVP , STRG ) ;
                      LVP -> . CCLASS := STRG ;
                      if LGTH > STRGLGTH then
                        begin
                          ERROR ( 399 ) ;
                          LGTH := STRGLGTH
                        end (* then *) ;
                      with LVP -> do
                        begin
                          SLGTH := LGTH ;
                          for I := 1 to LGTH do
                            SVAL [ I ] := STRNG [ I ]
                        end (* with *) ;
                      VAL . VALP := LVP
                    end (* else *)
                end (* tag/ca *) ;
         ':' : begin
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
         '.' : begin
                 OP := NOOP ;
                 NEXTCH ;
                 if CH = '.' then
                   begin
                     SY := COLON ;
                     NEXTCH
                   end (* then *)
                 else
                   SY := PERIOD
               end (* tag/ca *) ;
         '<' : begin
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
         '>' : begin
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
         '{' : begin
                 NEXTCH ;
                 SKIP := FALSE ;
                 if CH = '$' then
                   OPTIONS ;
                 if SKIP then
                   begin
                     READLN ;
                     LINECOUNT := SUCC ( LINECOUNT ) ;
                   end (* then *)
                 else
                   while CH <> '}' do
                     NEXTCH ;
                 NEXTCH ;
                 goto 1
               end (* tag/ca *) ;
         '(' : begin
                 NEXTCH ;
                 SKIP := FALSE ;
                 if CH = '*' then
                   begin
                     NEXTCH ;
                     if CH = '$' then
                       OPTIONS ;
                     if SKIP then
                       begin
                         READLN ;
                         LINECOUNT := SUCC ( LINECOUNT ) ;
                       end (* then *)
                     else
                       repeat
                         while CH <> '*' do
                           NEXTCH ;
                         NEXTCH
                       until CH = ')' ;
                     NEXTCH ;
                     goto 1
                   end (* then *) ;
                 SY := LPARENT ;
                 OP := NOOP
               end (* tag/ca *) ;
         '-' : begin
                 NEXTCH ;
                 if CH = '>' then
                   begin
                     NEXTCH ;
                     OP := NOOP ;
                     SY := ARROW
                   end (* then *)
                 else
                   begin
                     OP := MINUS ;
                     SY := ADDOP
                   end (* else *)
               end (* tag/ca *) ;
         '*' , '+' , '@' , '=' , '/' , ')' , '[' , ']' , ',' , ';' ,
         '^' : begin
                 SY := SSY [ CH ] ;
                 OP := SOP [ CH ] ;
                 NEXTCH
               end (* tag/ca *) ;
         ' ' : SY := OTHERSY ;
         otherwise
           
         : begin
             SY := OTHERSY ;
             OP := NOOP ;
             ERROR ( 399 ) ;
             NEXTCH ;
           end (* tag/ca *) ;
       end (* case *)
   end (* INSYMBOL *) ;



procedure ENTERID ( FCP : CTP ) ;

   var NAM : ALPHA ;
       LCP , LCP1 : CTP ;
       LLEFT : BOOLEAN ;

   begin (* ENTERID *)
     NAM := FCP -> . NAME ;
     LCP := DISPLAY [ TOP ] . FNAME ;
     if LCP = NIL then
       DISPLAY [ TOP ] . FNAME := FCP
     else
       begin
         repeat
           LCP1 := LCP ;
           if LCP -> . NAME = NAM then
             begin
               ERROR ( 101 ) ;
               LCP := LCP -> . RLINK ;
               LLEFT := FALSE
             end (* then *)
           else
             if LCP -> . NAME < NAM then
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
     FCP -> . RLINK := NIL ;
   end (* ENTERID *) ;



procedure SEARCHSECTION ( FCP : CTP ; var FCP1 : CTP ) ;

   label 1 ;

   begin (* SEARCHSECTION *)
     while FCP <> NIL do
       if FCP -> . NAME = ID then
         goto 1
       else
         if FCP -> . NAME < ID then
           FCP := FCP -> . RLINK
         else
           FCP := FCP -> . LLINK ;
     1 :
     FCP1 := FCP
   end (* SEARCHSECTION *) ;



procedure SEARCHID ( FIDCLS : SETOFIDS ; var FCP : CTP ) ;

   label 1 ;

   var LCP : CTP ;

   begin (* SEARCHID *)
     for DISX := TOP DOWNTO 0 do
       begin
         LCP := DISPLAY [ DISX ] . FNAME ;
         while LCP <> NIL do
           if LCP -> . NAME = ID then
             if LCP -> . KLASS in FIDCLS then
               goto 1
             else
               begin
                 if PRTERR then
                   ERROR ( 103 ) ;
                 LCP := LCP -> . RLINK
               end (* else *)
           else
             if LCP -> . NAME < ID then
               LCP := LCP -> . RLINK
             else
               LCP := LCP -> . LLINK
       end (* for *) ;
     if PRTERR then
       begin
         ERROR ( 104 ) ;
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
       end (* then *) ;
     1 :
     FCP := LCP
   end (* SEARCHID *) ;



procedure GETBOUNDS ( FSP : STP ; var FMIN , FMAX : INTEGER ) ;

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
                     else
                       ALIGNQUOT := PARMAL ;
           SUBRANGE :
             ALIGNQUOT := ALIGNQUOT ( RANGETYPE ) ;
           POINTER :
             ALIGNQUOT := ADRAL ;
           POWER : ALIGNQUOT := SETAL ;
           FILES : ALIGNQUOT := FILEAL ;
           ARRAYS :
             ALIGNQUOT := RECAL ;
           RECORDS :
             ALIGNQUOT := RECAL ;
           VARIANT , TAGFLD :
             ERROR ( 402 )
         end (* case *)
   end (* ALIGNQUOT *) ;



procedure ALIGN ( FSP : STP ; var FLC : INTEGER ) ;

   var K : INTEGER ;

   begin (* ALIGN *)
     K := ALIGNQUOT ( FSP ) ;
     if FLC >= 0 then
       begin
         FLC := ( FLC + ( K - 1 ) ) DIV K * K ;
       end (* then *)
     else
       begin
         FLC := ( FLC - ( K - 1 ) ) DIV K * K ;
       end (* else *) ;
   end (* ALIGN *) ;



procedure GENLABEL ( var NXTLAB : INTEGER ) ;

   begin (* GENLABEL *)
     INTLABEL := INTLABEL + 1 ;
     NXTLAB := INTLABEL
   end (* GENLABEL *) ;



procedure WRITESTRG ( var CNST : CONSTANT ) ;

   var I : INTEGER ;

   begin (* WRITESTRG *)
     with CNST do
       for I := 1 to SLGTH do
         WRITE ( PRR , SVAL [ I ] ) ;
   end (* WRITESTRG *) ;



procedure WRITENAME ( var NAME : ALPHA ) ;

   label 1 ;

   var I : INTEGER ;
       C : CHAR ;

   begin (* WRITENAME *)
     for I := 1 to ALPHALEN do
       begin
         C := NAME [ I ] ;
         if C = ' ' then
           goto 1 ;
         WRITE ( PRR , C : 1 ) ;
       end (* for *) ;
     1 :
     
   end (* WRITENAME *) ;



procedure BLOCK ( FSYS : SETOFSYS ; FSY : SYMBOL ; FPROCP : CTP ) ;

   var LSY : SYMBOL ;
       TEST : BOOLEAN ;
       LABELSDISP : DISPRANGE ;


   procedure SKIP ( FSYS : SETOFSYS ) ;

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
                if not EXPECTSTRING and ( LGTH = 1 ) then
                  LSP := CHARPTR
                else
                  begin
                    NEW ( LSP , ARRAYS ) ;
                    with LSP -> do
                      begin
                        AELTYPE := CHARPTR ;
                        INXTYPE := NIL ;
                        SIZE := LGTH * CHARSIZE + 1 ;
                        FORM := ARRAYS ;
                        VARY := TRUE ;
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
                                if FVALU . VALP -> . RVAL [ 1 ] = '-'
                                then
                                  LVP -> . RVAL [ 1 ] := '+'
                                else
                                  LVP -> . RVAL [ 1 ] := '-' ;
                                for I := 2 to RSTRGLGTH do
                                  LVP -> . RVAL [ I ] := FVALU . VALP
                                                   -> . RVAL [ I ] ;
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


   function EQUALBOUNDS ( FSP1 , FSP2 : STP ) : BOOLEAN ;

      var LMIN1 , LMIN2 , LMAX1 , LMAX2 : INTEGER ;

      begin (* EQUALBOUNDS *)
        if ( FSP1 = NIL ) or ( FSP2 = NIL ) then
          EQUALBOUNDS := TRUE
        else
          begin
            GETBOUNDS ( FSP1 , LMIN1 , LMAX1 ) ;
            GETBOUNDS ( FSP1 , LMIN2 , LMAX2 ) ;
            EQUALBOUNDS := ( LMIN1 = LMIN2 ) and ( LMAX1 = LMAX2 )
          end (* else *)
      end (* EQUALBOUNDS *) ;


   function COMPTYPES ( FSP1 , FSP2 : STP ) : BOOLEAN ;

      label 1 , 99 ;

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
                            begin
                              COMP := TRUE ;
                              goto 1 ;
                            end (* then *) ;
                          LTESTP1 := LASTTESTP
                        end (* with *) ;
                    NEW ( LTESTP1 ) ;
                    with LTESTP1 -> do
                      begin
                        ELT1 := FSP1 -> . ELTYPE ;
                        ELT2 := FSP2 -> . ELTYPE ;
                        LASTTESTP := GLOBTESTP
                      end (* with *) ;
                    GLOBTESTP := LTESTP1 ;
                    COMP := COMPTYPES ( FSP1 -> . ELTYPE , FSP2 -> .
                            ELTYPE ) ;
                    1 :
                    COMPTYPES := COMP ;
                    GLOBTESTP := LTESTP2 ;
                  end (* tag/ca *) ;
                POWER : COMPTYPES := COMPTYPES ( FSP1 -> . ELSET , FSP2
                                     -> . ELSET ) ;
                ARRAYS :
                  begin
                    COMP := COMPTYPES ( FSP1 -> . AELTYPE , FSP2 -> .
                            AELTYPE ) and COMPTYPES ( FSP1 -> . INXTYPE
                            , FSP2 -> . INXTYPE ) ;
                    if not FSP2 -> . VARY then
                      begin
                        if FSP1 -> . VARY then
                          COMP := COMP and ( FSP1 -> . SIZE > FSP2 -> .
                                  SIZE )
                        else
                          COMP := COMP and ( FSP1 -> . SIZE = FSP2 -> .
                                  SIZE ) and EQUALBOUNDS ( FSP1 -> .
                                  INXTYPE , FSP2 -> . INXTYPE ) ;
                      end (* then *) ;
                    COMPTYPES := COMP ;
                  end (* tag/ca *) ;
                RECORDS :
                  begin
                    NXT1 := FSP1 -> . FSTFLD ;
                    NXT2 := FSP2 -> . FSTFLD ;
                    COMP := TRUE ;
                    while ( NXT1 <> NIL ) and ( NXT2 <> NIL ) do
                      begin
                        COMP := COMP and COMPTYPES ( NXT1 -> . IDTYPE ,
                                NXT2 -> . IDTYPE ) ;
                        NXT1 := NXT1 -> . NEXT ;
                        NXT2 := NXT2 -> . NEXT
                      end (* while *) ;
                    COMPTYPES := COMP and ( NXT1 = NIL ) and ( NXT2 =
                                 NIL ) and ( FSP1 -> . RECVAR = NIL )
                                 and ( FSP2 -> . RECVAR = NIL )
                  end (* tag/ca *) ;
                FILES : COMPTYPES := COMPTYPES ( FSP1 -> . FILTYPE ,
                                     FSP2 -> . FILTYPE )
              end (* case *)
            else
              if FSP1 -> . FORM = SUBRANGE then
                COMPTYPES := COMPTYPES ( FSP1 -> . RANGETYPE , FSP2 )
              else
                if FSP2 -> . FORM = SUBRANGE then
                  COMPTYPES := COMPTYPES ( FSP1 , FSP2 -> . RANGETYPE )
                else
                  begin
                    if FSP1 -> . FORM = POINTER then
                      if FSP1 -> . ELTYPE <> NIL then
                        if ( FSP1 -> . ELTYPE -> . FORM = ARRAYS ) and
                        ( FSP2 -> . FORM = ARRAYS ) then
                          if FSP1 -> . ELTYPE -> . VARY and FSP2 -> .
                          VARY then
                            begin
                              COMPTYPES := TRUE ;
                              goto 99 ;
                            end (* then *) ;
                    COMPTYPES := FALSE
                  end (* else *)
          else
            COMPTYPES := TRUE ;
        99 :
        
      end (* COMPTYPES *) ;


   function ISSTRING ( FSP : STP ) : BOOLEAN ;

      begin (* ISSTRING *)
        ISSTRING := FALSE ;
        if FSP <> NIL then
          if FSP -> . FORM = ARRAYS then
            if FSP -> . AELTYPE = CHARPTR then
              ISSTRING := TRUE
      end (* ISSTRING *) ;


   procedure TYP ( FSYS : SETOFSYS ; var FSP : STP ; var FSIZE :
                 ADDRRANGE ) ;

      var LSP , LSP1 , LSP2 : STP ;
          OLDTOP : DISPRANGE ;
          LCP : CTP ;
          LSIZE , DISPL : ADDRRANGE ;
          LMIN , LMAX : INTEGER ;
          VALS : VALU ;


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
                   while DISPLAY [ TOP ] . OCCUR <> BLCK do
                     TOP := TOP - 1 ;
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
                               if ISSTRING ( RANGETYPE ) then
                                 begin
                                   ERROR ( 148 ) ;
                                   RANGETYPE := NIL
                                 end (* then *) ;
                               MIN := VALUES ;
                               SIZE := INTSIZE
                             end (* with *) ;
                           if SY = COLON then
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
                       LSP -> . FORM := SUBRANGE ;
                       CONSTANT ( FSYS + [ COLON ] , LSP1 , LVALU ) ;
                       if ISSTRING ( LSP1 ) then
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
                       if SY = COLON then
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

         var LCP , LCP1 , NXT , NXT1 , ROOT : CTP ;
             LSP , LSP1 , LSP2 , LSP3 , LSP4 : STP ;
             MINSIZE , MAXSIZE , LSIZE : ADDRRANGE ;
             LVALU : VALU ;

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
               NXT := NIL ;
               ROOT := NIL ;
               repeat
                 if SY = IDENT then
                   begin
                     NEW ( LCP , FIELD ) ;
                     with LCP -> do
                       begin
                         NAME := ID ;
                         IDTYPE := NIL ;
                         NEXT := NIL ;
                         KLASS := FIELD
                       end (* with *) ;
                     if NXT <> NIL then
                       NXT -> . NEXT := LCP
                     else
                       ROOT := LCP ;
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
               LCP -> . NEXT := NXT1 ;
               if SY = COLON then
                 INSYMBOL
               else
                 ERROR ( 5 ) ;
               TYP ( FSYS + [ CASESY , SEMICOLON ] , LSP , LSIZE ) ;
               NXT := ROOT ;
               while NXT <> NXT1 do
                 with NXT -> do
                   begin
                     ALIGN ( LSP , DISPL ) ;
                     IDTYPE := LSP ;
                     FLDADDR := DISPL ;
                     WRITE ( PRR , '; field ' ) ;
                     WRITENAME ( NAME ) ;
                     WRITELN ( PRR , ' disp:' , DISPL : 5 , ' size:' ,
                               LSIZE : 3 ) ;
                     NXT := NEXT ;
                     DISPL := DISPL + LSIZE
                   end (* with *) ;
               NXT1 := ROOT ;
               if SY = SEMICOLON then
                 begin
                   INSYMBOL ;
                   if not ( SY in [ IDENT , CASESY , ENDSY , RPARENT ]
                   ) then
                     begin
                       ERROR ( 19 ) ;
                       SKIP ( FSYS + [ IDENT , CASESY ] )
                     end (* then *)
                 end (* then *)
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
                   PRTERR := FALSE ;
                   SEARCHID ( [ TYPES ] , LCP1 ) ;
                   PRTERR := TRUE ;
                   if LCP1 <> NIL then
                     begin
                       LSP1 := LCP1 -> . IDTYPE ;
                       if LSP1 <> NIL then
                         if ( LSP1 -> . FORM <= SUBRANGE ) or ISSTRING
                         ( LSP1 ) then
                           begin
                             if REALPTR = LSP1 then
                               ERROR ( 109 )
                             else
                               if ISSTRING ( LSP1 ) then
                                 ERROR ( 399 ) ;
                             LSP -> . TAGFIELDP := LSP1 ;
                           end (* then *)
                         else
                           ERROR ( 110 ) ;
                       INSYMBOL ;
                     end (* then *)
                   else
                     begin
                       NEW ( LCP , FIELD ) ;
                       with LCP -> do
                         begin
                           NAME := ID ;
                           IDTYPE := NIL ;
                           KLASS := FIELD ;
                           NEXT := NIL ;
                           FLDADDR := DISPL
                         end (* with *) ;
                       ENTERID ( LCP ) ;
                       INSYMBOL ;
                       if SY = COLON then
                         INSYMBOL
                       else
                         ERROR ( 5 ) ;
                       if SY = IDENT then
                         begin
                           SEARCHID ( [ TYPES ] , LCP1 ) ;
                           LSP1 := LCP1 -> . IDTYPE ;
                           if LSP1 <> NIL then
                             begin
                               ALIGN ( LSP1 , DISPL ) ;
                               LCP -> . FLDADDR := DISPL ;
                               WRITE ( PRR , '; field ' ) ;
                               WRITENAME ( LCP -> . NAME ) ;
                               WRITELN ( PRR , ' disp:' , DISPL : 5 ,
                                         ' size:' , LSIZE : 3 ) ;
                               DISPL := DISPL + LSP1 -> . SIZE ;
                               if ( LSP1 -> . FORM <= SUBRANGE ) or
                               ISSTRING ( LSP1 ) then
                                 begin
                                   if REALPTR = LSP1 then
                                     ERROR ( 109 )
                                   else
                                     if ISSTRING ( LSP1 ) then
                                       ERROR ( 399 ) ;
                                   LCP -> . IDTYPE := LSP1 ;
                                   LSP -> . TAGFIELDP := LSP1 ;
                                 end (* then *)
                               else
                                 ERROR ( 110 ) ;
                             end (* then *) ;
                           INSYMBOL ;
                         end (* then *)
                       else
                         begin
                           ERROR ( 2 ) ;
                           SKIP ( FSYS + [ OFSY , LPARENT ] ) ;
                         end (* else *) ;
                     end (* else *) ;
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
                 if not ( SY in [ SEMICOLON , ENDSY ] ) then
                   begin
                     repeat
                       CONSTANT ( FSYS + [ COMMA , COLON , LPARENT ] ,
                                  LSP3 , LVALU ) ;
                       if LSP -> . TAGFIELDP <> NIL then
                         if not COMPTYPES ( LSP -> . TAGFIELDP , LSP3 )
                         then
                           ERROR ( 111 ) ;
                       NEW ( LSP3 , VARIANT ) ;
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
              if SY = ARROW then
                begin
                  NEW ( LSP , POINTER ) ;
                  FSP := LSP ;
                  with LSP -> do
                    begin
                      ELTYPE := NIL ;
                      SIZE := PTRSIZE ;
                      FORM := POINTER
                    end (* with *) ;
                  INSYMBOL ;
                  if SY = IDENT then
                    begin
                      PRTERR := FALSE ;
                      SEARCHID ( [ TYPES ] , LCP ) ;
                      PRTERR := TRUE ;
                      if LCP = NIL then
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
                  if SY = PACKEDSY then
                    begin
                      INSYMBOL ;
                      if not ( SY in TYPEDELS ) then
                        begin
                          ERROR ( 10 ) ;
                          SKIP ( FSYS + TYPEDELS )
                        end (* then *)
                    end (* then *) ;
                  if SY = STRINGSY then
                    begin
                      INSYMBOL ;
                      if SY = LBRACK then
                        INSYMBOL
                      else
                        ERROR ( 11 ) ;
                      NEW ( LSP , ARRAYS ) ;
                      with LSP -> do
                        begin
                          AELTYPE := CHARPTR ;
                          INXTYPE := NIL ;
                          FORM := ARRAYS ;
                          VARY := TRUE ;
                        end (* with *) ;
                      CONSTANT ( FSYS + [ RBRACK ] , LSP1 , VALS ) ;
                      if LSP1 = INTPTR then
                        begin
                          LSIZE := SUCC ( VALS . IVAL ) ;
                          LSP -> . SIZE := LSIZE ;
                          NEW ( LSP1 , SUBRANGE ) ;
                          with LSP1 -> do
                            begin
                              SIZE := INTSIZE ;
                              FORM := SUBRANGE ;
                              RANGETYPE := INTPTR ;
                              MIN . IVAL := 1 ;
                              MAX . IVAL := LSIZE ;
                            end (* with *) ;
                        end (* then *)
                      else
                        begin
                          ERROR ( 106 ) ;
                          LSP1 := NIL
                        end (* else *) ;
                      LSP -> . INXTYPE := LSP1 ;
                      if SY = RBRACK then
                        INSYMBOL
                      else
                        ERROR ( 12 ) ;
                    end (* then *)
                  else
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
                          with LSP -> do
                            begin
                              AELTYPE := LSP1 ;
                              INXTYPE := NIL ;
                              FORM := ARRAYS ;
                              VARY := FALSE ;
                            end (* with *) ;
                          LSP1 := LSP ;
                          SIMPLETYPE ( FSYS + [ COMMA , RBRACK , OFSY ]
                                       , LSP2 , LSIZE ) ;
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
                                  LSIZE := LSIZE * ( LMAX - LMIN + 1 )
                                           ;
                                  SIZE := LSIZE
                                end (* then *)
                            end (* with *) ;
                          LSP := LSP1 ;
                          LSP1 := LSP2
                        until LSP1 = NIL
                      end (* then *)
                    else
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
                                  OCCUR := REC
                                end (* with *)
                            end (* then *)
                          else
                            ERROR ( 250 ) ;
                          DISPL := 0 ;
                          FIELDLIST ( FSYS - [ SEMICOLON ] + [ ENDSY ]
                                      , LSP1 ) ;
                          NEW ( LSP , RECORDS ) ;
                          with LSP -> do
                            begin
                              FSTFLD := DISPLAY [ TOP ] . FNAME ;
                              RECVAR := LSP1 ;
                              SIZE := DISPL ;
                              FORM := RECORDS
                            end (* with *) ;
                          TOP := OLDTOP ;
                          if SY = ENDSY then
                            INSYMBOL
                          else
                            ERROR ( 13 )
                        end (* then *)
                      else
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
                                  ERROR ( 114 ) ;
                            NEW ( LSP , POWER ) ;
                            with LSP -> do
                              begin
                                ELSET := LSP1 ;
                                SIZE := SETSIZE ;
                                FORM := POWER
                              end (* with *) ;
                          end (* then *)
                        else
                          if SY = FILESY then
                            begin
                              INSYMBOL ;
                              if SY = OFSY then
                                INSYMBOL
                              else
                                ERROR ( 8 ) ;
                              NEW ( LSP , FILES ) ;
                              TYP ( FSYS , LSP1 , LSIZE ) ;
                              with LSP -> do
                                begin
                                  FORM := FILES ;
                                  FILTYPE := LSP1 ;
                                  SIZE := LSIZE + FCBSIZE ;
                                end (* with *) ;
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
          REDEF : BOOLEAN ;
          LBNAME : INTEGER ;

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
            with LCP -> do
              begin
                NAME := ID ;
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
            repeat
              WRITELN ( OUTPUT , ' TYPE-ID ' , FWPTR -> . NAME ) ;
              FWPTR := FWPTR -> . NEXT
            until FWPTR = NIL ;
            if not EOL then
              WRITE ( OUTPUT , ' ' : CHCNT + 16 )
          end (* then *)
      end (* TYPEDECLARATION *) ;


   procedure VARDECLARATION ;

      var LCP , BFR , ROOT , FCP : CTP ;
          LSP : STP ;
          LSIZE : ADDRRANGE ;
          VALS : VALU ;
          XNAME : CSP ;

      begin (* VARDECLARATION *)
        ROOT := NIL ;
        BFR := NIL ;
        repeat
          repeat
            if SY = IDENT then
              begin
                NEW ( LCP , VARS ) ;
                with LCP -> do
                  begin
                    if BFR <> NIL then
                      BFR -> . NEXT := LCP
                    else
                      ROOT := LCP ;
                    NAME := ID ;
                    NEXT := NIL ;
                    KLASS := VARS ;
                    IDTYPE := NIL ;
                    VKIND := ACTUAL ;
                    VLEV := LEVEL ;
                    VXNAME := NIL ;
                    VIMPORT := FALSE ;
                  end (* with *) ;
                BFR := LCP ;
                ENTERID ( LCP ) ;
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
          TYP ( FSYS + [ SEMICOLON , IMPORTSY , EXPORTSY ] + TYPEDELS ,
                LSP , LSIZE ) ;
          if LSP -> . FORM = FILES then
            if LEVEL > 1 then
              ERROR ( 399 )
            else
              begin
                if FILECP = NIL then
                  FILECP := LCP
                else
                  begin
                    FCP := FILECP ;
                    while FCP -> . NEXT <> NIL do
                      FCP := FCP -> . NEXT ;
                    FCP -> . NEXT := LCP ;
                  end (* else *) ;
              end (* else *) ;
          BFR := ROOT ;
          while BFR <> NIL do
            with BFR -> do
              begin
                IDTYPE := LSP ;
                if ( SY = IMPORTSY ) then
                  VADDR := 0
                else
                  begin
                    if FPROCP = NIL then
                      begin
                        ALIGN ( LSP , LC ) ;
                        VADDR := LC ;
                        LC := LC + LSIZE ;
                      end (* then *)
                    else
                      begin
                        LC := LC - LSIZE ;
                        ALIGN ( LSP , LC ) ;
                        VADDR := LC ;
                      end (* else *) ;
                    WRITE ( PRR , '; var   ' ) ;
                    WRITENAME ( NAME ) ;
                    WRITELN ( PRR , ' addr:' , VADDR : 5 , ' size:' ,
                              LSIZE : 3 ) ;
                  end (* else *) ;
                BFR := NEXT ;
              end (* with *) ;
          if SY = IMPORTSY then
            begin
              INSYMBOL ;
              XNAME := NIL ;
              if SY = LPARENT then
                begin
                  EXPECTSTRING := TRUE ;
                  INSYMBOL ;
                  CONSTANT ( FSYS + [ RPARENT ] , LSP , VALS ) ;
                  EXPECTSTRING := FALSE ;
                  if LSP <> NIL then
                    if LSP -> . FORM = ARRAYS then
                      XNAME := VALS . VALP
                    else
                      ERROR ( 23 )
                  else
                    ERROR ( 22 ) ;
                  if SY = RPARENT then
                    INSYMBOL
                  else
                    ERROR ( 4 ) ;
                end (* then *) ;
              BFR := ROOT ;
              while BFR <> NIL do
                with BFR -> do
                  begin
                    VIMPORT := TRUE ;
                    VXNAME := XNAME ;
                    VKIND := FORMAL ;
                    BFR := NEXT ;
                  end (* with *) ;
            end (* then *)
          else
            if SY = EXPORTSY then
              begin
                BFR := ROOT ;
                if LEVEL > 1 then
                  begin
                    BFR := NIL ;
                    ERROR ( 399 ) ;
                  end (* then *) ;
                while BFR <> NIL do
                  with BFR -> do
                    begin
                      WRITE ( PRR , '@' ) ;
                      WRITENAME ( NAME ) ;
                      WRITELN ( PRR , '= ' , VADDR : 1 , '#GLOBAL' ) ;
                      BFR := NEXT ;
                    end (* with *) ;
                INSYMBOL ;
              end (* then *) ;
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
        if FWPTR <> NIL then
          begin
            ERROR ( 117 ) ;
            WRITELN ( OUTPUT ) ;
            repeat
              WRITELN ( OUTPUT , ' TYPE-ID ' , FWPTR -> . NAME ) ;
              FWPTR := FWPTR -> . NEXT
            until FWPTR = NIL ;
          end (* then *)
      end (* VARDECLARATION *) ;


   procedure PROCDECLARATION ( FSY : SYMBOL ) ;

      type P_TO_INT = -> INTEGER ;

      var OLD_MIN_LEVEL , OLDLEV : 0 .. MAXLEVEL ;
          LSY : SYMBOL ;
          LCP , LCP1 : CTP ;
          LSP : STP ;
          FORW : BOOLEAN ;
          OLDTOP : DISPRANGE ;
          PARM_COUNT , FRESSIZE : INTEGER ;
          LLC , LCM : ADDRRANGE ;
          LBNAME : INTEGER ;
          MARKP : -> INTEGER ;
          VALS : VALU ;


      procedure MARK ( PTR : P_TO_INT ) ;

         begin (* MARK *)
           
         end (* MARK *) ;


      procedure RELEASE ( PTR : P_TO_INT ) ;

         begin (* RELEASE *)
           
         end (* RELEASE *) ;


      procedure PARAMETERLIST ( FSY : SETOFSYS ; var FPAR : CTP ) ;

         var LCP , LCP1 , LCP2 , LCP3 : CTP ;
             LSP : STP ;
             LKIND : IDKIND ;
             LLC : ADDRRANGE ;
             COUNT , LSIZE : INTEGER ;

         begin (* PARAMETERLIST *)
           LCP1 := NIL ;
           PARM_COUNT := 0 ;
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
                       ERROR ( 399 ) ;
                       repeat
                         INSYMBOL ;
                         if SY = IDENT then
                           begin
                             NEW ( LCP , PROC , DECLARED , FORMAL ) ;
                             with LCP -> do
                               begin
                                 NAME := ID ;
                                 IDTYPE := NIL ;
                                 NEXT := LCP1 ;
                                 PFLEV := LEVEL ;
                                 KLASS := PROC ;
                                 PFDECKIND := DECLARED ;
                                 PFKIND := FORMAL
                               end (* with *) ;
                             ENTERID ( LCP ) ;
                             LCP1 := LCP ;
                             ALIGN ( PARMPTR , LC ) ;
                             INSYMBOL
                           end (* then *)
                         else
                           ERROR ( 2 ) ;
                         if not ( SY in FSYS + [ COMMA , SEMICOLON ,
                         RPARENT ] ) then
                           begin
                             ERROR ( 7 ) ;
                             SKIP ( FSYS + [ COMMA , SEMICOLON ,
                                    RPARENT ] )
                           end (* then *)
                       until SY <> COMMA
                     end (* then *)
                   else
                     begin
                       if SY = FUNCSY then
                         begin
                           ERROR ( 399 ) ;
                           LCP2 := NIL ;
                           repeat
                             INSYMBOL ;
                             if SY = IDENT then
                               begin
                                 NEW ( LCP , FUNC , DECLARED , FORMAL )
                                       ;
                                 with LCP -> do
                                   begin
                                     NAME := ID ;
                                     IDTYPE := NIL ;
                                     NEXT := LCP2 ;
                                     PFLEV := LEVEL ;
                                     KLASS := FUNC ;
                                     PFDECKIND := DECLARED ;
                                     PFKIND := FORMAL
                                   end (* with *) ;
                                 ENTERID ( LCP ) ;
                                 LCP2 := LCP ;
                                 ALIGN ( PARMPTR , LC ) ;
                                 INSYMBOL ;
                               end (* then *) ;
                             if not ( SY in [ COMMA , COLON ] + FSYS )
                             then
                               begin
                                 ERROR ( 7 ) ;
                                 SKIP ( FSYS + [ COMMA , SEMICOLON ,
                                        RPARENT ] )
                               end (* then *)
                           until SY <> COMMA ;
                           if SY = COLON then
                             begin
                               INSYMBOL ;
                               if SY = IDENT then
                                 begin
                                   SEARCHID ( [ TYPES ] , LCP ) ;
                                   LSP := LCP -> . IDTYPE ;
                                   if LSP <> NIL then
                                     if not ( LSP -> . FORM in [ SCALAR
                                     , SUBRANGE , POINTER ] ) then
                                       begin
                                         ERROR ( 120 ) ;
                                         LSP := NIL
                                       end (* then *) ;
                                   LCP3 := LCP2 ;
                                   while LCP2 <> NIL do
                                     begin
                                       LCP2 -> . IDTYPE := LSP ;
                                       LCP := LCP2 ;
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
                             ERROR ( 5 )
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
                                 with LCP -> do
                                   begin
                                     NAME := ID ;
                                     IDTYPE := NIL ;
                                     KLASS := VARS ;
                                     VKIND := LKIND ;
                                     NEXT := LCP2 ;
                                     VLEV := LEVEL ;
                                     VXNAME := NIL ;
                                     VIMPORT := FALSE ;
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
                                       if LSP -> . FORM = FILES then
                                         ERROR ( 121 ) ;
                                   LCP3 := LCP2 ;
                                   while LCP2 <> NIL do
                                     begin
                                       LCP := LCP2 ;
                                       with LCP2 -> do
                                         begin
                                           PARM_COUNT := SUCC (
                                                   PARM_COUNT ) ;
                                           IDTYPE := LSP ;
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
               LLC := LCBEFOREMARKSTACK ;
               while LCP1 <> NIL do
                 with LCP1 -> do
                   begin
                     LCP2 := NEXT ;
                     NEXT := LCP3 ;
                     ALIGN ( PARMPTR , LLC ) ;
                     if KLASS = VARS then
                       if IDTYPE <> NIL then
                         if ( VKIND = ACTUAL ) and ( IDTYPE -> . FORM >
                         POWER ) then
                           begin
                             LC := LC - IDTYPE -> . SIZE ;
                             ALIGN ( IDTYPE , LC ) ;
                             VADDR := LC ;
                           end (* then *)
                         else
                           if ( VKIND = ACTUAL ) and ( ( IDTYPE =
                           BOOLPTR ) or ( IDTYPE = CHARPTR ) ) then
                             VADDR := LLC + 3
                           else
                             VADDR := LLC
                       else
                         VADDR := LLC ;
                     if IDTYPE -> . FORM = POWER then
                       begin
                         LLC := LLC + SETSIZE ;
                         PARM_COUNT := PRED ( PARM_COUNT ) + SETSIZE
                                       DIV PARMSIZE ;
                       end (* then *)
                     else
                       LLC := LLC + PTRSIZE ;
                     WRITE ( PRR , '; parm: ' ) ;
                     WRITENAME ( NAME ) ;
                     WRITELN ( PRR , ' addr: ' , VADDR : 5 ) ;
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
                with LCP -> do
                  begin
                    NAME := ID ;
                    IDTYPE := NIL ;
                    PFXNAME := NIL ;
                    PF_ATTR := INTERN ;
                    PFLEV := LEVEL ;
                    GENLABEL ( LBNAME ) ;
                    PFDECKIND := DECLARED ;
                    PFKIND := ACTUAL ;
                    PFNAME := LBNAME ;
                    NUMB_OF_PARM := 0 ;
                    if FSY = PROCSY then
                      KLASS := PROC
                    else
                      KLASS := FUNC
                  end (* with *) ;
                ENTERID ( LCP )
              end (* then *)
            else
              begin
                LCP1 := LCP -> . NEXT ;
                while LCP1 <> NIL do
                  begin
                    with LCP1 -> do
                      if KLASS = VARS then
                        if IDTYPE <> NIL then
                          begin
                            LCM := VADDR ;
                            if LCM < LC then
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
        if LEVEL < MAXLEVEL then
          LEVEL := LEVEL + 1
        else
          ERROR ( 251 ) ;
        OLD_MIN_LEVEL := MIN_LEVEL ;
        if TOP < DISPLIMIT then
          begin
            TOP := TOP + 1 ;
            with DISPLAY [ TOP ] do
              begin
                if FORW then
                  FNAME := LCP -> . NEXT
                else
                  FNAME := NIL ;
                FLABEL := NIL ;
                OCCUR := BLCK
              end (* with *)
          end (* then *)
        else
          ERROR ( 250 ) ;
        if FSY = PROCSY then
          begin
            PARAMETERLIST ( [ SEMICOLON ] , LCP1 ) ;
            if not FORW then
              begin
                LCP -> . NEXT := LCP1 ;
                LCP -> . NUMB_OF_PARM := PARM_COUNT ;
              end (* then *) ;
          end (* then *)
        else
          begin
            PARAMETERLIST ( [ SEMICOLON , COLON ] , LCP1 ) ;
            if not FORW then
              begin
                LCP -> . NEXT := LCP1 ;
                LCP -> . NUMB_OF_PARM := PARM_COUNT ;
              end (* then *) ;
            GENLABEL ( LCP -> . RESULT_LABEL ) ;
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
                      if LSP -> . FORM >= POWER then
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
        if SY = IMPORTSY then
          begin
            LCP -> . PF_ATTR := IMPRT ;
            INSYMBOL ;
            if SY = LPARENT then
              begin
                EXPECTSTRING := TRUE ;
                INSYMBOL ;
                CONSTANT ( FSYS + [ RPARENT ] , LSP , VALS ) ;
                EXPECTSTRING := FALSE ;
                if LSP <> NIL then
                  if LSP -> . FORM = ARRAYS then
                    LCP -> . PFXNAME := VALS . VALP
                  else
                    ERROR ( 23 )
                else
                  ERROR ( 22 ) ;
                if SY = RPARENT then
                  INSYMBOL
                else
                  ERROR ( 4 ) ;
              end (* then *) ;
            if SY = SEMICOLON then
              INSYMBOL
            else
              ERROR ( 14 ) ;
            if not ( SY in FSYS ) then
              begin
                ERROR ( 6 ) ;
                SKIP ( FSYS ) ;
              end (* then *) ;
          end (* then *)
        else
          begin
            if SY = FORWARDSY then
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
                MARK ( MARKP ) ;
                if PRCODE then
                  begin
                    WRITE ( PRR , '; PROC ' ) ;
                    WRITENAME ( LCP -> . NAME ) ;
                    WRITELN ( PRR ) ;
                  end (* then *) ;
                repeat
                  BLOCK ( FSYS , SEMICOLON , LCP ) ;
                  if SY = SEMICOLON then
                    begin
                      INSYMBOL ;
                      if not ( SY in [ BEGINSY , PROCSY , FUNCSY ] )
                      then
                        begin
                          ERROR ( 6 ) ;
                          SKIP ( FSYS )
                        end (* then *)
                    end (* then *)
                  else
                    ERROR ( 14 )
                until ( SY in [ BEGINSY , PROCSY , FUNCSY ] ) or EOF (
                INPUT ) ;
                GLOBTESTP := NIL ;
                RELEASE ( MARKP ) ;
              end (* else *) ;
          end (* else *) ;
        MIN_LEVEL := OLD_MIN_LEVEL ;
        LEVEL := OLDLEV ;
        TOP := OLDTOP ;
        LC := LLC ;
      end (* PROCDECLARATION *) ;


   procedure BODY ( FSYS : SETOFSYS ) ;

      const CSTOCCMAX = 2 ;
            CIXMAX = 1000 ;

      type OPRANGE = 0 .. 67 ;

      var LLCP : CTP ;
          SAVEID : ALPHA ;
          CSTPTRIX : 0 .. CSTOCCMAX ;
          I , ENTNAME , SEGSIZE : INTEGER ;
          STACKTOP , TOPNEW , TOPMAX : INTEGER ;
          FCBADDR , LCMAX , LLC1 : ADDRRANGE ;
          FCP , LCP : CTP ;
          LLP : LBP ;
          CSTPTR : array [ 1 .. CSTOCCMAX ] of CSP ;


      procedure MES ( I : INTEGER ) ;

         begin (* MES *)
           TOPNEW := TOPNEW + CDX [ I ] * MAXSTACK ;
           if TOPNEW > TOPMAX then
             TOPMAX := TOPNEW
         end (* MES *) ;


      procedure PPUTLC ;

         begin (* PPUTLC *)
           if LINECOUNT <> OLDLINECOUNT then
             begin
               WRITELN ( PRR , '=' , LINECOUNT : 1 ) ;
               OLDLINECOUNT := LINECOUNT ;
             end (* then *) ;
         end (* PPUTLC *) ;


      procedure PUTLC ;

         begin (* PUTLC *)
           if DEBUG then
             PPUTLC
           else
             if LINECOUNT - OLDLINECOUNT >= 10 then
               PPUTLC ;
         end (* PUTLC *) ;


      procedure GEN0 ( FOP : OPRANGE ) ;

         begin (* GEN0 *)
           if PRCODE then
             begin
               PUTLC ;
               WRITELN ( PRR , MN [ FOP ] : 4 )
             end (* then *) ;
           IC := IC + 1 ;
           MES ( FOP )
         end (* GEN0 *) ;


      procedure GEN1 ( FOP : OPRANGE ; FP2 : INTEGER ) ;

         var K : INTEGER ;

         begin (* GEN1 *)
           if PRCODE then
             begin
               PUTLC ;
               WRITE ( PRR , MN [ FOP ] : 4 ) ;
               if FOP = 30 then
                 begin
                   WRITELN ( PRR , SNA [ FP2 ] : 5 ) ;
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
                           WRITE ( PRR , ' ' , SLGTH : 1 , ' "' ) ;
                           for K := 1 to SLGTH - 1 do
                             begin
                               WRITE ( PRR , ORD ( SVAL [ K ] ) : 1 ,
                                       ',' ) ;
                             end (* for *) ;
                           WRITELN ( PRR , ORD ( SVAL [ SLGTH ] ) : 1 ,
                                     '"' ) ;
                         end (* with *) ;
                     end (* then *)
                   else
                     if FOP = 42 then
                       WRITELN ( PRR , CHR ( FP2 ) )
                     else
                       WRITELN ( PRR , ' ' , FP2 : 1 ) ;
                   MES ( FOP )
                 end (* else *)
             end (* then *) ;
           IC := IC + 1
         end (* GEN1 *) ;


      procedure GEN2 ( FOP : OPRANGE ; FP1 , FP2 : INTEGER ) ;

         var L , K : INTEGER ;

         begin (* GEN2 *)
           if PRCODE then
             begin
               PUTLC ;
               WRITE ( PRR , MN [ FOP ] : 4 ) ;
               case FOP of
                 41 , 45 , 50 , 54 , 56 :
                   WRITELN ( PRR , ' ' , FP1 : 1 , ' ' , FP2 : 1 ) ;
                 47 , 48 , 49 , 52 , 53 , 55 :
                   begin
                     WRITE ( PRR , CHR ( FP1 ) ) ;
                     if ( CHR ( FP1 ) = 'M' ) or ( CHR ( FP1 ) = 'V' )
                     then
                       WRITE ( PRR , ' ' , FP2 : 1 ) ;
                     WRITELN ( PRR )
                   end (* tag/ca *) ;
                 51 : case FP1 of
                        1 : WRITELN ( PRR , 'I ' , FP2 : 1 ) ;
                        2 : begin
                              WRITE ( PRR , 'R ' ) ;
                              with CSTPTR [ FP2 ] -> do
                                for K := 1 to RSTRGLGTH do
                                  WRITE ( PRR , RVAL [ K ] ) ;
                              WRITELN ( PRR )
                            end (* tag/ca *) ;
                        3 : WRITELN ( PRR , 'B ' , FP2 : 1 ) ;
                        4 : WRITELN ( PRR , 'N' ) ;
                        6 : WRITELN ( PRR , 'C ''' : 3 , CHR ( FP2 ) ,
                                      '''' ) ;
                        5 : begin
                              WRITE ( PRR , '(' ) ;
                              L := - 1 ;
                              with CSTPTR [ FP2 ] -> do
                                begin
                                  K := SETLOW ;
                                  repeat
                                    if K in PVAL then
                                      begin
                                        L := K ;
                                        repeat
                                          K := SUCC ( K ) ;
                                        until not ( K in PVAL ) or ( K
                                        > SETHIGH ) ;
                                        K := PRED ( K ) ;
                                        if K = L then
                                          WRITE ( PRR , ' ' , K : 1 )
                                        else
                                          WRITE ( PRR , ' ' , L : 1 ,
                                                  '-' , K : 1 ) ;
                                      end (* then *) ;
                                    K := SUCC ( K ) ;
                                  until K > SETHIGH ;
                                end (* with *) ;
                              WRITELN ( PRR , ')' )
                            end (* tag/ca *)
                      end (* case *)
               end (* case *) ;
             end (* then *) ;
           IC := IC + 1 ;
           MES ( FOP )
         end (* GEN2 *) ;


      procedure GEN3 ( FOP : OPRANGE ; FP1 , FP2 , FP3 : INTEGER ) ;

         var K : INTEGER ;

         begin (* GEN3 *)
           if PRCODE then
             begin
               PUTLC ;
               WRITELN ( PRR , MN [ FOP ] : 4 , ' ' , FP1 : 1 , ' ' ,
                         FP2 : 1 , ' ' , FP3 : 1 ) ;
             end (* then *) ;
           IC := SUCC ( IC ) ;
           MES ( FOP ) ;
         end (* GEN3 *) ;


      procedure GENTYPINDICATOR ( FSP : STP ) ;

         begin (* GENTYPINDICATOR *)
           if FSP <> NIL then
             with FSP -> do
               case FORM of
                 SCALAR :
                   if FSP = INTPTR then
                     WRITE ( PRR , 'I' )
                   else
                     if FSP = BOOLPTR then
                       WRITE ( PRR , 'B' )
                     else
                       if FSP = CHARPTR then
                         WRITE ( PRR , 'C' )
                       else
                         if SCALKIND = DECLARED then
                           WRITE ( PRR , 'I' )
                         else
                           WRITE ( PRR , 'R ' ) ;
                 SUBRANGE :
                   GENTYPINDICATOR ( RANGETYPE ) ;
                 FILES , POINTER :
                   WRITE ( PRR , 'A' ) ;
                 POWER : WRITE ( PRR , 'S' ) ;
                 ARRAYS :
                   begin
                     if VARY then
                       WRITE ( PRR , 'V' )
                     else
                       WRITE ( PRR , 'M' ) ;
                   end (* tag/ca *) ;
                 RECORDS :
                   WRITE ( PRR , 'M' ) ;
                 TAGFLD , VARIANT :
                   ERROR ( 401 )
               end (* case *) ;
           WRITE ( PRR , ' ' ) ;
         end (* GENTYPINDICATOR *) ;


      procedure GEN0T ( FOP : OPRANGE ; FSP : STP ) ;

         begin (* GEN0T *)
           if PRCODE then
             begin
               PUTLC ;
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
               PUTLC ;
               WRITE ( PRR , MN [ FOP ] : 4 ) ;
               GENTYPINDICATOR ( FSP ) ;
               WRITELN ( PRR , FP2 : 1 )
             end (* then *) ;
           IC := IC + 1 ;
           MES ( FOP )
         end (* GEN1T *) ;


      procedure GEN2T ( FOP : OPRANGE ; FP1 , FP2 : INTEGER ; FSP : STP
                      ) ;

         begin (* GEN2T *)
           if PRCODE then
             begin
               PUTLC ;
               WRITE ( PRR , MN [ FOP ] : 4 ) ;
               GENTYPINDICATOR ( FSP ) ;
               WRITELN ( PRR , FP1 : 1 , ' ' , FP2 : 1 ) ;
             end (* then *) ;
           IC := IC + 1 ;
           MES ( FOP )
         end (* GEN2T *) ;


      procedure GEN2TL ( FOP : OPRANGE ; FP1 , FP2 : INTEGER ; FSP :
                       STP ) ;

         begin (* GEN2TL *)
           if PRCODE then
             begin
               PUTLC ;
               WRITE ( PRR , MN [ FOP ] : 4 ) ;
               GENTYPINDICATOR ( FSP ) ;
               WRITELN ( PRR , FP1 : 1 , ' &' , FP2 : 1 ) ;
             end (* then *) ;
           IC := IC + 1 ;
           MES ( FOP )
         end (* GEN2TL *) ;


      procedure LOAD ;

         begin (* LOAD *)
           with GATTR do
             if TYPTR <> NIL then
               begin
                 case KIND of
                   CST : if ( TYPTR -> . FORM = SCALAR ) and ( TYPTR <>
                         REALPTR ) then
                           if TYPTR = BOOLPTR then
                             GEN2 ( 51 , 3 , CVAL . IVAL )
                           else
                             if TYPTR = CHARPTR then
                               GEN2 ( 51 , 6 , CVAL . IVAL )
                             else
                               GEN2 ( 51 , 1 , CVAL . IVAL )
                         else
                           if TYPTR = NILPTR then
                             GEN2 ( 51 , 4 , 0 )
                           else
                             if CSTPTRIX >= CSTOCCMAX then
                               ERROR ( 254 )
                             else
                               begin
                                 CSTPTRIX := 1 ;
                                 CSTPTR [ CSTPTRIX ] := CVAL . VALP ;
                                 if TYPTR = REALPTR then
                                   GEN2 ( 51 , 2 , CSTPTRIX )
                                 else
                                   GEN2 ( 51 , 5 , CSTPTRIX )
                               end (* else *) ;
                   VARBL : begin
                             case ACCESS of
                               DRCT : begin
                                        if VLEVEL <= 1 then
                                          GEN1T ( 39 , DPLMT , TYPTR )
                                        else
                                          GEN2T ( 54 , LEVEL - VLEVEL ,
                                                  DPLMT , TYPTR ) ;
                                        if PRCODE then
                                          begin
                                            WRITE ( PRR , '; load ' ) ;
                                            WRITENAME ( ACP -> . NAME )
                                                   ;
                                            WRITELN ( PRR ) ;
                                          end (* then *) ;
                                      end (* tag/ca *) ;
                               INDRCT :
                                 GEN1T ( 35 , IDPLMT , TYPTR ) ;
                               INXD : ERROR ( 400 )
                             end (* case *) ;
                           end (* tag/ca *) ;
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
                 DRCT : begin
                          if VLEVEL <= 1 then
                            GEN1T ( 43 , DPLMT , TYPTR )
                          else
                            if LABEL_ACCESS then
                              GEN2TL ( 56 , LEVEL - VLEVEL , DPLMT ,
                                       TYPTR )
                            else
                              GEN2T ( 56 , LEVEL - VLEVEL , DPLMT ,
                                      TYPTR ) ;
                          if PRCODE then
                            begin
                              WRITE ( PRR , '; store ' ) ;
                              WRITENAME ( ACP -> . NAME ) ;
                              WRITELN ( PRR ) ;
                            end (* then *) ;
                        end (* tag/ca *) ;
                 INDRCT :
                   if IDPLMT <> 0 then
                     ERROR ( 400 )
                   else
                     GEN1T ( 26 , 0 , TYPTR ) ;
                 INXD : ERROR ( 400 )
               end (* case *) ;
         end (* STORE *) ;


      procedure LOADADDRESS ;

         begin (* LOADADDRESS *)
           with GATTR do
             if TYPTR <> NIL then
               begin
                 case KIND of
                   CST : if ISSTRING ( TYPTR ) then
                           if CSTPTRIX >= CSTOCCMAX then
                             ERROR ( 254 )
                           else
                             begin
                               CSTPTRIX := 1 ;
                               CSTPTR [ CSTPTRIX ] := CVAL . VALP ;
                               GEN1 ( 38 , CSTPTRIX )
                             end (* else *)
                         else
                           ERROR ( 400 ) ;
                   VARBL : case ACCESS of
                             DRCT : begin
                                      if VLEVEL <= 1 then
                                        GEN1 ( 37 , DPLMT )
                                      else
                                        GEN2 ( 50 , LEVEL - VLEVEL ,
                                               DPLMT ) ;
                                      if PRCODE then
                                        begin
                                          WRITE ( PRR , '; loadaddr ' )
                                                  ;
                                          WRITENAME ( ACP -> . NAME ) ;
                                          WRITELN ( PRR ) ;
                                        end (* then *) ;
                                    end (* tag/ca *) ;
                             INDRCT :
                               if IDPLMT <> 0 then
                                 GEN1T ( 34 , IDPLMT , NILPTR ) ;
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
               PUTLC ;
               WRITE ( PRR , MN [ 33 ] : 4 ) ;
               WRITELN ( PRR , ' &' , FADDR : 1 )
             end (* then *) ;
           IC := IC + 1 ;
           MES ( 33 )
         end (* GENFJP *) ;


      procedure GENUJP ( FOP : OPRANGE ; FP2 : INTEGER ) ;

         begin (* GENUJP *)
           if PRCODE then
             begin
               PUTLC ;
               WRITE ( PRR , MN [ FOP ] : 4 ) ;
               WRITELN ( PRR , ' &' , FP2 : 1 )
             end (* then *) ;
           IC := IC + 1 ;
           MES ( FOP )
         end (* GENUJP *) ;


      procedure GENJPA ( FP1 , FP2 : INTEGER ) ;

         begin (* GENJPA *)
           if PRCODE then
             WRITELN ( PRR , ' JPA ' , '&' , FP1 : 1 , ' &' , FP2 : 1 )
                       ;
         end (* GENJPA *) ;


      procedure GENXJP ( FOP : OPRANGE ; MAXI , LAB : INTEGER ) ;

         begin (* GENXJP *)
           if PRCODE then
             begin
               PUTLC ;
               WRITE ( PRR , MN [ FOP ] : 4 ) ;
               WRITELN ( PRR , ' ' , MAXI : 1 , ' &' , LAB : 1 ) ;
             end (* then *) ;
           IC := SUCC ( IC ) ;
           MES ( FOP ) ;
         end (* GENXJP *) ;


      procedure GENCUPENT ( FOP : OPRANGE ; FP1 , FP2 : INTEGER ) ;

         begin (* GENCUPENT *)
           if PRCODE then
             begin
               PUTLC ;
               WRITE ( PRR , MN [ FOP ] : 4 ) ;
               WRITE ( PRR , ' ' , FP1 : 1 , ' &' , FP2 : 1 ) ;
               if FOP = 46 then
                 if GATTR . ACP = NIL then
                   WRITE ( PRR , ' ' , '_MAIN' )
                 else
                   begin
                     WRITE ( PRR , ' ' ) ;
                     WRITENAME ( GATTR . ACP -> . NAME ) ;
                   end (* else *) ;
               WRITELN ( PRR ) ;
             end (* then *) ;
           IC := IC + 1 ;
           MES ( FOP )
         end (* GENCUPENT *) ;


      procedure CHECKBNDS ( FSP : STP ) ;

         var LMIN , LMAX : INTEGER ;

         begin (* CHECKBNDS *)
           if FSP <> NIL then
             if FSP <> INTPTR then
               if FSP <> REALPTR then
                 if FSP -> . FORM <= SUBRANGE then
                   begin
                     GETBOUNDS ( FSP , LMIN , LMAX ) ;
                     GEN2T ( 45 , LMIN , LMAX , FSP )
                   end (* then *)
         end (* CHECKBNDS *) ;


      procedure PUTLABEL ( LABNAME : INTEGER ) ;

         begin (* PUTLABEL *)
           if PRCODE then
             WRITELN ( PRR , '&' , LABNAME : 1 )
         end (* PUTLABEL *) ;


      procedure STATEMENT ( FSYS : SETOFSYS ) ;

         label 1 ;

         var LCP : CTP ;
             LLP : LBP ;


         procedure EXPRESSION ( FSYS : SETOFSYS ) ;

            FORWARD ;


         procedure SELECTOR ( FSYS : SETOFSYS ; FCP : CTP ) ;

            label 1 ;

            var LATTR : ATTR ;
                LCP : CTP ;
                LSIZE , LMIN , LMAX : INTEGER ;
                I : INTEGER ;
                C : CHAR ;

            begin (* SELECTOR *)
              with FCP -> , GATTR do
                begin
                  TYPTR := IDTYPE ;
                  KIND := VARBL ;
                  LABEL_ACCESS := FALSE ;
                  case KLASS of
                    VARS : if VKIND = ACTUAL then
                             begin
                               ACCESS := DRCT ;
                               VLEVEL := VLEV ;
                               DPLMT := VADDR ;
                               ACP := FCP ;
                             end (* then *)
                           else
                             begin
                               if VIMPORT then
                                 begin
                                   if PRCODE then
                                     begin
                                       WRITE ( PRR , ' LXA ' ) ;
                                       if VXNAME <> NIL then
                                         WRITESTRG ( VXNAME -> )
                                       else
                                         begin
                                           WRITE ( PRR , '_' ) ;
                                           for I := 1 to ALPHALEN do
                                             begin
                                               C := NAME [ I ] ;
                                               if C = ' ' then
                                                 goto 1 ;
                                               if ( C >= 'A' ) and ( C
                                               <= 'Z' ) then
                                                 C := CHR ( ORD ( C ) +
                                                   32 ) ;
                                               WRITE ( PRR , C : 1 ) ;
                                             end (* for *) ;
                                         end (* else *) ;
                                       1 :
                                       WRITELN ( PRR ) ;
                                     end (* then *) ;
                                 end (* then *)
                               else
                                 GEN2T ( 54 , LEVEL - VLEV , VADDR ,
                                         NILPTR ) ;
                               ACCESS := INDRCT ;
                               IDPLMT := 0 ;
                               ACP := FCP ;
                             end (* else *) ;
                    FIELD : with DISPLAY [ DISX ] do
                              if OCCUR = CREC then
                                begin
                                  ACCESS := DRCT ;
                                  VLEVEL := CLEV ;
                                  DPLMT := CDSPL + FLDADDR ;
                                  ACP := FNAME ;
                                end (* then *)
                              else
                                begin
                                  if LEVEL = 1 then
                                    GEN1T ( 39 , VDSPL , NILPTR )
                                  else
                                    GEN2T ( 54 , 0 , VDSPL , NILPTR ) ;
                                  ACCESS := INDRCT ;
                                  IDPLMT := FLDADDR ;
                                  ACP := FNAME ;
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
                                 if ( PFLEV + 1 <> LEVEL ) or ( FPROCP
                                 <> FCP ) then
                                   ERROR ( 177 ) ;
                               begin
                                 ACCESS := DRCT ;
                                 VLEVEL := PFLEV + 1 ;
                                 LABEL_ACCESS := TRUE ;
                                 DPLMT := FPROCP -> . RESULT_LABEL ;
                                 ACP := FCP ;
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
                            if GATTR . TYPTR <> INTPTR then
                              GEN0T ( 58 , GATTR . TYPTR ) ;
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
                                        GEN2T ( 45 , LMIN , LMAX ,
                                                INTPTR ) ;
                                      if LMIN > 0 then
                                        GEN1T ( 31 , LMIN , INTPTR )
                                      else
                                        if LMIN < 0 then
                                          GEN1T ( 34 , - LMIN , INTPTR
                                                  ) ;
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
                                  GEN1 ( 36 , LSIZE )
                                end (* then *)
                            end (* with *)
                      until SY <> COMMA ;
                      if SY = RBRACK then
                        INSYMBOL
                      else
                        ERROR ( 12 )
                    end (* then *)
                  else
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
                      begin
                        if GATTR . TYPTR <> NIL then
                          with GATTR , TYPTR -> do
                            begin
                              LOAD ;
                              if FORM = POINTER then
                                TYPTR := ELTYPE
                              else
                                if FORM = FILES then
                                  TYPTR := FILTYPE
                                else
                                  ERROR ( 141 ) ;
                              if DEBUG then
                                GEN2T ( 45 , 1 , MAXADDR , NILPTR ) ;
                              with GATTR do
                                begin
                                  KIND := VARBL ;
                                  ACCESS := INDRCT ;
                                  IDPLMT := 0
                                end (* with *)
                            end (* with *) ;
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

            var LKEY : 1 .. 15 ;


            procedure VARIABLE ( FSYS : SETOFSYS ) ;

               var LCP : CTP ;

               begin (* VARIABLE *)
                 if SY = IDENT then
                   begin
                     SEARCHID ( [ VARS , FIELD ] , LCP ) ;
                     INSYMBOL ;
                     if LCP <> NIL then
                       if LCP -> . KLASS = VARS then
                         if LCP -> . VLEV < MIN_LEVEL then
                           ERROR ( 59 ) ;
                   end (* then *)
                 else
                   begin
                     ERROR ( 2 ) ;
                     LCP := UVARPTR
                   end (* else *) ;
                 SELECTOR ( FSYS , LCP )
               end (* VARIABLE *) ;


            procedure GETPUTRESETREWRITE ;

               var FCP : CTP ;
                   SLEN , I : INTEGER ;

               begin (* GETPUTRESETREWRITE *)
                 VARIABLE ( FSYS + [ RPARENT , COMMA ] ) ;
                 if GATTR . TYPTR = NIL then
                   begin
                     if ( LKEY = 1 ) or ( LKEY = 3 ) then
                       FCP := FILECP -> . NEXT
                     else
                       FCP := FILECP ;
                     GEN1 ( 37 , FCP -> . VADDR ) ;
                     GATTR . TYPTR := FCP -> . IDTYPE ;
                     GATTR . ACP := FCP ;
                   end (* then *)
                 else
                   begin
                     FCP := GATTR . ACP ;
                     LOADADDRESS ;
                     if GATTR . TYPTR -> . FORM <> FILES then
                       begin
                         ERROR ( 116 ) ;
                         GATTR . TYPTR := FILECP -> . IDTYPE ;
                       end (* then *) ;
                   end (* else *) ;
                 if LKEY <= 2 then
                   begin
                     if SY = COMMA then
                       begin
                         ERROR ( 4 ) ;
                         SKIP ( FSYS + [ RPARENT ] ) ;
                       end (* then *) ;
                     GEN1 ( 30 , LKEY )
                   end (* then *)
                 else
                   begin
                     if GATTR . TYPTR = TEXTPTR then
                       GEN2 ( 51 , 1 , 0 )
                     else
                       GEN2 ( 51 , 1 , GATTR . TYPTR -> . FILTYPE -> .
                              SIZE ) ;
                     if SY = COMMA then
                       begin
                         INSYMBOL ;
                         SLEN := 0 ;
                         EXPRESSION ( FSYS + [ RPARENT ] ) ;
                         if not ISSTRING ( GATTR . TYPTR ) then
                           ERROR ( 116 )
                         else
                           with GATTR do
                             begin
                               if KIND = CST then
                                 SLEN := CVAL . VALP -> . SLGTH
                               else
                                 SLEN := TYPTR -> . SIZE ;
                             end (* with *) ;
                         GEN2 ( 51 , 1 , SLEN ) ;
                         LOADADDRESS ;
                       end (* then *)
                     else
                       if FCP = FILECP then
                         begin
                           WRITELN ( PRR , ' LDCI     1  ; output' ) ;
                           GEN2 ( 51 , 1 , 0 ) ;
                         end (* then *)
                       else
                         if FCP = FILECP -> . NEXT then
                           begin
                             WRITELN ( PRR , ' LDCI     0  ; input' ) ;
                             GEN2 ( 51 , 1 , 0 ) ;
                           end (* then *)
                         else
                           with FCP -> do
                             begin
                               SLEN := 0 ;
                               for I := 1 to ALPHALEN do
                                 if NAME [ I ] <> ' ' then
                                   SLEN := SUCC ( SLEN ) ;
                               GEN2 ( 51 , 1 , SLEN + 4 ) ;
                               WRITE ( PRR , ' LCA''' ) ;
                               for I := 1 to SLEN do
                                 WRITE ( PRR , NAME [ I ] ) ;
                               WRITELN ( PRR , '.dat''' ) ;
                             end (* with *) ;
                     GEN1 ( 30 , LKEY + 21 ) ;
                   end (* else *)
               end (* GETPUTRESETREWRITE *) ;


            procedure SREAD ;

               var LCP , FCP : CTP ;
                   LLEV : LEVRANGE ;
                   LADDR : ADDRRANGE ;
                   LSP : STP ;
                   N : INTEGER ;

               begin (* SREAD *)
                 LLEV := 1 ;
                 LADDR := FILECP -> . NEXT -> . VADDR ;
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
                             if FILTYPE = CHARPTR then
                               begin
                                 LLEV := VLEVEL ;
                                 LADDR := DPLMT ;
                               end (* then *)
                             else
                               ERROR ( 399 ) ;
                             if SY = RPARENT then
                               begin
                                 if LKEY = 8 then
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
                           end (* with *) ;
                     if not TEST then
                       repeat
                         LOADADDRESS ;
                         GEN2 ( 50 , LEVEL - LLEV , LADDR ) ;
                         if GATTR . TYPTR <> NIL then
                           if GATTR . TYPTR -> . FORM <= SUBRANGE then
                             if COMPTYPES ( INTPTR , GATTR . TYPTR )
                             then
                               GEN1 ( 30 , 3 )
                             else
                               if COMPTYPES ( REALPTR , GATTR . TYPTR )
                               then
                                 GEN1 ( 30 , 4 )
                               else
                                 if CHARPTR = GATTR . TYPTR then
                                   GEN1 ( 30 , 5 )
                                 else
                                   ERROR ( 399 )
                           else
                             if ISSTRING ( GATTR . TYPTR ) then
                               begin
                                 N := GATTR . TYPTR -> . SIZE ;
                                 if GATTR . TYPTR -> . VARY then
                                   N := - N ;
                                 GEN2 ( 51 , 1 , N ) ;
                                 GEN1 ( 30 , 6 ) ;
                               end (* then *)
                             else
                               ERROR ( 116 ) ;
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
                   if LKEY = 5 then
                     ERROR ( 116 ) ;
                 if LKEY = 11 then
                   begin
                     GEN2 ( 50 , LEVEL - LLEV , LADDR ) ;
                     GEN1 ( 30 , 21 )
                   end (* then *)
               end (* SREAD *) ;


            procedure SWRITE ;

               var LSP : STP ;
                   DEFAULT : BOOLEAN ;
                   LLKEY : 1 .. 15 ;
                   LCP : CTP ;
                   LLEV : LEVRANGE ;
                   LADDR , LEN : ADDRRANGE ;

               begin (* SWRITE *)
                 LLKEY := LKEY ;
                 LLEV := 1 ;
                 LADDR := FILECP -> . VADDR ;
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
                             if FILTYPE = CHARPTR then
                               begin
                                 LLEV := VLEVEL ;
                                 LADDR := DPLMT
                               end (* then *)
                             else
                               ERROR ( 399 ) ;
                             if SY = RPARENT then
                               begin
                                 if LLKEY = 10 then
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
                           end (* with *) ;
                     if not TEST then
                       repeat
                         LSP := GATTR . TYPTR ;
                         if LSP <> NIL then
                           if LSP -> . FORM <= SUBRANGE then
                             LOAD
                           else
                             LOADADDRESS ;
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
                             EXPRESSION ( FSYS + [ COMMA , RPARENT ] )
                                          ;
                             if GATTR . TYPTR <> NIL then
                               if GATTR . TYPTR <> INTPTR then
                                 ERROR ( 116 ) ;
                             if LSP <> REALPTR then
                               ERROR ( 124 ) ;
                             LOAD ;
                             ERROR ( 399 ) ;
                           end (* then *)
                         else
                           if LSP = INTPTR then
                             begin
                               if DEFAULT then
                                 GEN2 ( 51 , 1 , 1 ) ;
                               GEN2 ( 50 , LEVEL - LLEV , LADDR ) ;
                               GEN1 ( 30 , 7 )
                             end (* then *)
                           else
                             if LSP = REALPTR then
                               begin
                                 if DEFAULT then
                                   GEN2 ( 51 , 1 , 1 ) ;
                                 GEN2 ( 50 , LEVEL - LLEV , LADDR ) ;
                                 GEN1 ( 30 , 8 )
                               end (* then *)
                             else
                               if LSP = CHARPTR then
                                 begin
                                   if DEFAULT then
                                     GEN2 ( 51 , 1 , 1 ) ;
                                   GEN2 ( 50 , LEVEL - LLEV , LADDR ) ;
                                   GEN1 ( 30 , 9 )
                                 end (* then *)
                               else
                                 if LSP <> NIL then
                                   begin
                                     if LSP -> . FORM = SCALAR then
                                       ERROR ( 399 )
                                     else
                                       if ISSTRING ( LSP ) then
                                         begin
                                           LEN := LSP -> . SIZE DIV
                                                  CHARMAX ;
                                           if LSP -> . VARY then
                                             LEN := PRED ( LEN ) ;
                                           if DEFAULT then
                                             GEN2 ( 51 , 1 , 0 ) ;
                                           GEN2 ( 51 , 1 , LEN ) ;
                                           GEN2 ( 50 , LEVEL - LLEV ,
                                                  LADDR ) ;
                                           GEN1 ( 30 , 10 )
                                         end (* then *)
                                       else
                                         ERROR ( 116 )
                                   end (* then *) ;
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
                   if LKEY = 6 then
                     ERROR ( 116 ) ;
                 if LLKEY = 12 then
                   begin
                     GEN2 ( 50 , LEVEL - LLEV , LADDR ) ;
                     GEN1 ( 30 , 22 )
                   end (* then *)
               end (* SWRITE *) ;


            procedure SPACK ;

               var LSP , LSP1 : STP ;

               begin (* SPACK *)
                 ERROR ( 399 ) ;
                 VARIABLE ( FSYS + [ COMMA , RPARENT ] ) ;
                 LSP := NIL ;
                 LSP1 := NIL ;
                 if GATTR . TYPTR <> NIL then
                   with GATTR . TYPTR -> do
                     if FORM = ARRAYS then
                       begin
                         LSP := INXTYPE ;
                         LSP1 := AELTYPE
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
                       ERROR ( 116 ) ;
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
                       end (* then *)
                     else
                       ERROR ( 116 )
               end (* SPACK *) ;


            procedure SUNPACK ;

               var LSP , LSP1 : STP ;

               begin (* SUNPACK *)
                 ERROR ( 399 ) ;
                 VARIABLE ( FSYS + [ COMMA , RPARENT ] ) ;
                 LSP := NIL ;
                 LSP1 := NIL ;
                 if GATTR . TYPTR <> NIL then
                   with GATTR . TYPTR -> do
                     if FORM = ARRAYS then
                       begin
                         LSP := INXTYPE ;
                         LSP1 := AELTYPE
                       end (* then *)
                     else
                       ERROR ( 116 ) ;
                 if SY = COMMA then
                   INSYMBOL
                 else
                   ERROR ( 20 ) ;
                 VARIABLE ( FSYS + [ COMMA , RPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then
                   with GATTR . TYPTR -> do
                     if FORM = ARRAYS then
                       begin
                         if not COMPTYPES ( AELTYPE , LSP1 ) or not
                         COMPTYPES ( INXTYPE , LSP ) then
                           ERROR ( 116 )
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
                       ERROR ( 116 ) ;
               end (* SUNPACK *) ;


            procedure SNEW ;

               label 1 ;

               var LSP , LSP1 : STP ;
                   VARTS , LMIN , LMAX : INTEGER ;
                   LSIZE , LSZ : ADDRRANGE ;
                   LVAL : VALU ;

               begin (* SNEW *)
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
                     if LSP = NIL then
                       ERROR ( 158 )
                     else
                       if LSP -> . FORM <> TAGFLD then
                         ERROR ( 162 )
                       else
                         if LSP -> . TAGFIELDP <> NIL then
                           if ISSTRING ( LSP1 ) or ( LSP1 = REALPTR )
                           then
                             ERROR ( 159 )
                           else
                             if COMPTYPES ( LSP -> . TAGFIELDP , LSP1 )
                             then
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
                 GEN2 ( 51 , 1 , LSIZE ) ;
                 GEN1 ( 30 , 12 ) ;
               end (* SNEW *) ;


            procedure SMARK ;

               begin (* SMARK *)
                 VARIABLE ( FSYS + [ RPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM = POINTER then
                     begin
                       LOADADDRESS ;
                       GEN1 ( 30 , 23 )
                     end (* then *)
                   else
                     ERROR ( 125 )
               end (* SMARK *) ;


            procedure SRELEASE ;

               begin (* SRELEASE *)
                 VARIABLE ( FSYS + [ RPARENT ] ) ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM = POINTER then
                     begin
                       LOAD ;
                       GEN1 ( 30 , 13 )
                     end (* then *)
                   else
                     ERROR ( 125 )
               end (* SRELEASE *) ;


            procedure SABS ;

               begin (* SABS *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR = INTPTR then
                     GEN0 ( 0 )
                   else
                     if GATTR . TYPTR = REALPTR then
                       GEN0 ( 1 )
                     else
                       begin
                         ERROR ( 125 ) ;
                         GATTR . TYPTR := INTPTR
                       end (* else *)
               end (* SABS *) ;


            procedure SSQR ;

               begin (* SSQR *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR = INTPTR then
                     GEN0 ( 24 )
                   else
                     if GATTR . TYPTR = REALPTR then
                       GEN0 ( 25 )
                     else
                       begin
                         ERROR ( 125 ) ;
                         GATTR . TYPTR := INTPTR
                       end (* else *)
               end (* SSQR *) ;


            procedure STRUNC ;

               begin (* STRUNC *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> REALPTR then
                     ERROR ( 125 ) ;
                 GEN0 ( 27 ) ;
                 GATTR . TYPTR := INTPTR
               end (* STRUNC *) ;


            procedure SROUND ;

               begin (* SROUND *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> REALPTR then
                     ERROR ( 125 ) ;
                 WRITELN ( PRR , ' CSP  RND' ) ;
                 GATTR . TYPTR := INTPTR
               end (* SROUND *) ;


            procedure SODD ;

               begin (* SODD *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> INTPTR then
                     ERROR ( 125 ) ;
                 GEN0 ( 20 ) ;
                 GATTR . TYPTR := BOOLPTR
               end (* SODD *) ;


            procedure SORD ;

               begin (* SORD *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM >= POWER then
                     ERROR ( 125 ) ;
                 GEN0T ( 58 , GATTR . TYPTR ) ;
                 GATTR . TYPTR := INTPTR
               end (* SORD *) ;


            procedure SCHR ;

               begin (* SCHR *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR <> INTPTR then
                     ERROR ( 125 ) ;
                 GEN0 ( 59 ) ;
                 GATTR . TYPTR := CHARPTR
               end (* SCHR *) ;


            procedure PREDSUCC ;

               var N : INTEGER ;
                   SP : STP ;

               begin (* PREDSUCC *)
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM > POINTER then
                     ERROR ( 125 ) ;
                 SP := GATTR . TYPTR ;
                 N := 1 ;
                 if GATTR . TYPTR -> . FORM = POINTER then
                   begin
                     SP := GATTR . TYPTR -> . ELTYPE ;
                     N := SP -> . SIZE ;
                   end (* then *) ;
                 if LKEY = 7 then
                   GEN1T ( 31 , N , SP )
                 else
                   GEN1T ( 34 , N , SP )
               end (* PREDSUCC *) ;


            procedure SEOF ;

               begin (* SEOF *)
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
                   with GATTR do
                     begin
                       TYPTR := TEXTPTR ;
                       KIND := VARBL ;
                       ACCESS := DRCT ;
                       VLEVEL := 1 ;
                       DPLMT := FILECP -> . NEXT -> . VADDR ;
                     end (* with *) ;
                 LOADADDRESS ;
                 if GATTR . TYPTR <> NIL then
                   if GATTR . TYPTR -> . FORM <> FILES then
                     ERROR ( 125 ) ;
                 if LKEY = 9 then
                   GEN0 ( 8 )
                 else
                   GEN1 ( 30 , 14 ) ;
                 GATTR . TYPTR := BOOLPTR
               end (* SEOF *) ;


            procedure SSIZEOF ;

               var CP : CTP ;

               begin (* SSIZEOF *)
                 if SY = LPARENT then
                   INSYMBOL
                 else
                   ERROR ( 9 ) ;
                 SEARCHID ( [ TYPES , VARS ] , CP ) ;
                 INSYMBOL ;
                 if CP <> NIL then
                   if CP -> . IDTYPE <> NIL then
                     GEN2 ( 51 , 1 , CP -> . IDTYPE -> . SIZE ) ;
                 GATTR . TYPTR := INTPTR ;
               end (* SSIZEOF *) ;


            procedure SADDROF ;

               var SP : STP ;

               begin (* SADDROF *)
                 if SY = LPARENT then
                   INSYMBOL
                 else
                   ERROR ( 9 ) ;
                 VARIABLE ( FSYS + [ RPARENT ] ) ;
                 LOADADDRESS ;
                 if GATTR . TYPTR <> NIL then
                   begin
                     NEW ( SP , POINTER ) ;
                     with SP -> do
                       begin
                         SIZE := PTRSIZE ;
                         FORM := POINTER ;
                         ELTYPE := GATTR . TYPTR ;
                       end (* with *) ;
                     GATTR . TYPTR := SP ;
                   end (* then *)
               end (* SADDROF *) ;


            procedure CALLNONSTANDARD ;

               label 1 ;

               var NXT , LCP : CTP ;
                   LSP : STP ;
                   LKIND : IDKIND ;
                   LB : BOOLEAN ;
                   LLC : ADDRRANGE ;
                   LOCPAR , FRESSIZE , I : INTEGER ;
                   C : CHAR ;

               begin (* CALLNONSTANDARD *)
                 LOCPAR := 0 ;
                 with FCP -> do
                   begin
                     NXT := NEXT ;
                     LKIND := PFKIND ;
                     if PFLEV < MIN_LEVEL then
                       ERROR ( 59 ) ;
                   end (* with *) ;
                 if SY = LPARENT then
                   begin
                     LLC := LC ;
                     repeat
                       LB := FALSE ;
                       if LKIND = ACTUAL then
                         begin
                           if ( NXT = NIL ) and ( FCP -> . PF_ATTR <>
                           IMPRT ) then
                             ERROR ( 126 )
                           else
                             LB := NXT -> . KLASS in [ PROC , FUNC ]
                         end (* then *)
                       else
                         ERROR ( 399 ) ;
                       if LB then
                         begin
                           INSYMBOL ;
                           ERROR ( 399 ) ;
                           if SY <> IDENT then
                             begin
                               ERROR ( 2 ) ;
                               SKIP ( FSYS + [ COMMA , RPARENT ] )
                             end (* then *)
                           else
                             begin
                               if NXT -> . KLASS = PROC then
                                 SEARCHID ( [ PROC ] , LCP )
                               else
                                 begin
                                   SEARCHID ( [ FUNC ] , LCP ) ;
                                   if not COMPTYPES ( LCP -> . IDTYPE ,
                                   NXT -> . IDTYPE ) then
                                     ERROR ( 128 )
                                 end (* else *) ;
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
                             EXPECTSTRING := ( NXT -> . IDTYPE -> .
                                             FORM = ARRAYS ) ;
                           INSYMBOL ;
                           EXPRESSION ( FSYS + [ COMMA , RPARENT ] ) ;
                           EXPECTSTRING := FALSE ;
                           if GATTR . TYPTR <> NIL then
                             if LKIND = ACTUAL then
                               begin
                                 LOCPAR := SUCC ( LOCPAR ) ;
                                 if NXT <> NIL then
                                   begin
                                     LSP := NXT -> . IDTYPE ;
                                     if LSP <> NIL then
                                       begin
                                         if ( NXT -> . VKIND = ACTUAL )
                                         then
                                           if LSP -> . FORM <= POWER
                                           then
                                             begin
                                               if LSP -> . FORM = POWER
                                               then
                                                 LOCPAR := PRED (
                                                   LOCPAR ) + SETSIZE
                                                   DIV PARMSIZE ;
                                               LOAD ;
                                               if DEBUG then
                                                 CHECKBNDS ( LSP ) ;
                                               if ( GATTR . TYPTR =
                                               INTPTR ) and ( LSP =
                                               REALPTR ) then
                                                 begin
                                                   GEN0 ( 10 ) ;
                                                   GATTR . TYPTR :=
                                                   REALPTR
                                                 end (* then *)
                                               else
                                                 if LSP -> . FORM <=
                                                 SUBRANGE then
                                                   GEN0T ( 58 , GATTR .
                                                   TYPTR ) ;
                                             end (* then *)
                                           else
                                             begin
                                               LOADADDRESS ;
                                             end (* else *)
                                         else
                                           if GATTR . KIND = VARBL then
                                             begin
                                               LOADADDRESS ;
                                             end (* then *)
                                           else
                                             ERROR ( 154 ) ;
                                         if not COMPTYPES ( LSP , GATTR
                                         . TYPTR ) then
                                           ERROR ( 142 )
                                       end (* then *)
                                   end (* then *)
                                 else
                                   with GATTR . TYPTR -> do
                                     if FORM <= POWER then
                                       begin
                                         LOAD ;
                                         if FORM <= SUBRANGE then
                                           GEN0T ( 59 , GATTR . TYPTR )
                                                   ;
                                       end (* then *)
                                     else
                                       LOADADDRESS ;
                               end (* then *)
                             else
                               begin
                                 
                               end (* else *)
                         end (* else *) ;
                       if ( LKIND = ACTUAL ) and ( NXT <> NIL ) then
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
                     if ( NXT <> NIL ) then
                       ERROR ( 126 ) ;
                     GATTR . ACP := FCP ;
                     with FCP -> do
                       begin
                         if PRCODE and ( PF_ATTR <> INTERN ) then
                           begin
                             WRITE ( PRR , ' CXP  ' , LOCPAR : 2 , ' '
                                     ) ;
                             if PFXNAME <> NIL then
                               WRITESTRG ( PFXNAME -> )
                             else
                               begin
                                 WRITE ( PRR , '_' ) ;
                                 I := 1 ;
                                 for I := 1 to ALPHALEN do
                                   begin
                                     C := NAME [ I ] ;
                                     if C = ' ' then
                                       goto 1 ;
                                     if ( C >= 'A' ) and ( C <= 'Z' )
                                     then
                                       C := CHR ( ORD ( C ) + 32 ) ;
                                     WRITE ( PRR , C : 1 ) ;
                                   end (* for *) ;
                               end (* else *) ;
                             1 :
                             WRITELN ( PRR ) ;
                           end (* then *)
                         else
                           GENCUPENT ( 46 , LEVEL - PFLEV , PFNAME ) ;
                         GEN1T ( 41 , LOCPAR * PTRSIZE , IDTYPE ) ;
                       end (* with *)
                   end (* then *) ;
                 GATTR . TYPTR := FCP -> . IDTYPE
               end (* CALLNONSTANDARD *) ;


            begin (* CALL *)
              if FCP -> . PFDECKIND = STANDARD then
                begin
                  LKEY := FCP -> . KEY ;
                  if FCP -> . KLASS = PROC then
                    begin
                      if not ( LKEY in [ 5 , 6 , 11 , 12 ] ) then
                        if SY = LPARENT then
                          INSYMBOL
                        else
                          ERROR ( 9 ) ;
                      case LKEY of
                        1 , 2 , 3 , 4 :
                          GETPUTRESETREWRITE ;
                        5 , 11 :
                          SREAD ;
                        6 , 12 :
                          SWRITE ;
                        7 : SPACK ;
                        8 : SUNPACK ;
                        9 : SNEW ;
                        10 : SRELEASE ;
                        13 : SMARK
                      end (* case *) ;
                      if not ( LKEY in [ 5 , 6 , 11 , 12 ] ) then
                        if SY = RPARENT then
                          INSYMBOL
                        else
                          ERROR ( 4 )
                    end (* then *)
                  else
                    begin
                      if ( LKEY <= 8 ) or ( LKEY = 11 ) then
                        begin
                          if SY = LPARENT then
                            INSYMBOL
                          else
                            ERROR ( 9 ) ;
                          EXPRESSION ( FSYS + [ RPARENT ] ) ;
                          LOAD
                        end (* then *) ;
                      case LKEY of
                        1 : SABS ;
                        2 : SSQR ;
                        3 : STRUNC ;
                        4 : SODD ;
                        5 : SORD ;
                        6 : SCHR ;
                        7 , 8 : PREDSUCC ;
                        9 , 10 :
                          SEOF ;
                        11 : SROUND ;
                        12 : SSIZEOF ;
                        13 : SADDROF ;
                      end (* case *) ;
                      if ( LKEY <= 8 ) or ( LKEY >= 11 ) then
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
                LSIZE , GSIZE : ADDRRANGE ;


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
                         CSTPART : set of SETLOW .. SETHIGH ;
                         LSP , LSP1 : STP ;
                         VALS : VALU ;
                         LOW , HIGH : INTEGER ;

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
                                       SEARCHID ( [ KONST , VARS ,
                                                  FIELD , FUNC ] , LCP
                                                  ) ;
                                       INSYMBOL ;
                                       GATTR . ACP := LCP ;
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
                                             if LCP -> . KLASS = VARS
                                             then
                                               if LCP -> . VLEV <
                                               MIN_LEVEL then
                                                 ERROR ( 59 ) ;
                                             SELECTOR ( FSYS , LCP ) ;
                                             if GATTR . TYPTR <> NIL
                                             then
                                               with GATTR , TYPTR -> do
                                                 if FORM = SUBRANGE
                                                 then
                                                   TYPTR := RANGETYPE
                                           end (* else *)
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
                                     if not EXPECTSTRING and ( LGTH = 1
                                     ) then
                                       TYPTR := CHARPTR
                                     else
                                       begin
                                         NEW ( LSP , ARRAYS ) ;
                                         with LSP -> do
                                           begin
                                             AELTYPE := CHARPTR ;
                                             FORM := ARRAYS ;
                                             INXTYPE := NIL ;
                                             SIZE := LGTH * CHARSIZE +
                                                   1 ;
                                             VARY := TRUE ;
                                           end (* with *) ;
                                         TYPTR := LSP
                                       end (* else *) ;
                                     KIND := CST ;
                                     CVAL := VAL
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
                                 CSTPART := [ ] ;
                                 VARPART := FALSE ;
                                 NEW ( LSP , POWER ) ;
                                 with LSP -> do
                                   begin
                                     ELSET := NIL ;
                                     SIZE := SETSIZE ;
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
                                                   RBRACK , COLON ] ) ;
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
                                               if GATTR . KIND = CST
                                               then
                                                 begin
                                                   LOW := GATTR . CVAL
                                                   . IVAL ;
                                                   HIGH := LOW ;
                                                   if SY = COLON then
                                                   begin
                                                   INSYMBOL ;
                                                   CONSTANT ( FSYS + [
                                                   COMMA , RBRACK ] ,
                                                   LSP1 , VALS ) ;
                                                   if LSP1 <> NIL then
                                                   if LSP1 -> . FORM =
                                                   SCALAR then
                                                   HIGH := VALS . IVAL
                                                   else
                                                   ERROR ( 136 )
                                                   else
                                                   ERROR ( 106 ) ;
                                                   end (* then *) ;
                                                   if ( LOW <= HIGH )
                                                   and ( LOW >= SETLOW
                                                   ) and ( HIGH <=
                                                   SETHIGH ) then
                                                   for LOW := LOW to
                                                   HIGH do
                                                   CSTPART := CSTPART +
                                                   [ LOW ]
                                                   else
                                                   ERROR ( 304 ) ;
                                                 end (* then *)
                                               else
                                                 begin
                                                   if SY = COLON then
                                                   ERROR ( 106 ) ;
                                                   LOAD ;
                                                   if not COMPTYPES (
                                                   GATTR . TYPTR ,
                                                   INTPTR ) then
                                                   GEN0T ( 58 , GATTR .
                                                   TYPTR ) ;
                                                   GEN0 ( 23 ) ;
                                                   if VARPART then
                                                   GEN0 ( 28 )
                                                   else
                                                   VARPART := TRUE
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
                                         LVP -> . PVAL := CSTPART ;
                                         LVP -> . CCLASS := PSET ;
                                         if CSTPTRIX = CSTOCCMAX then
                                           ERROR ( 254 )
                                         else
                                           begin
                                             CSTPTRIX := 1 ;
                                             CSTPTR [ CSTPTRIX ] := LVP
                                                   ;
                                             GEN2 ( 51 , 5 , CSTPTRIX )
                                                   ;
                                             GEN0 ( 28 ) ;
                                             GATTR . KIND := EXPR
                                           end (* else *)
                                       end (* then *)
                                   end (* then *)
                                 else
                                   begin
                                     NEW ( LVP , PSET ) ;
                                     LVP -> . PVAL := CSTPART ;
                                     LVP -> . CCLASS := PSET ;
                                     GATTR . CVAL . VALP := LVP
                                   end (* else *)
                               end (* tag/ca *)
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
                            MUL : if ( LATTR . TYPTR = INTPTR ) and (
                                  GATTR . TYPTR = INTPTR ) then
                                    GEN0 ( 15 )
                                  else
                                    begin
                                      if LATTR . TYPTR = INTPTR then
                                        begin
                                          GEN0 ( 9 ) ;
                                          LATTR . TYPTR := REALPTR
                                        end (* then *)
                                      else
                                        if GATTR . TYPTR = INTPTR then
                                          begin
                                            GEN0 ( 10 ) ;
                                            GATTR . TYPTR := REALPTR
                                          end (* then *) ;
                                      if ( LATTR . TYPTR = REALPTR )
                                      and ( GATTR . TYPTR = REALPTR )
                                      then
                                        GEN0 ( 16 )
                                      else
                                        if ( LATTR . TYPTR -> . FORM =
                                        POWER ) and COMPTYPES ( LATTR .
                                        TYPTR , GATTR . TYPTR ) then
                                          GEN0 ( 12 )
                                        else
                                          begin
                                            ERROR ( 134 ) ;
                                            GATTR . TYPTR := NIL
                                          end (* else *)
                                    end (* else *) ;
                            RDIV : begin
                                     if GATTR . TYPTR = INTPTR then
                                       begin
                                         GEN0 ( 10 ) ;
                                         GATTR . TYPTR := REALPTR
                                       end (* then *) ;
                                     if LATTR . TYPTR = INTPTR then
                                       begin
                                         GEN0 ( 9 ) ;
                                         LATTR . TYPTR := REALPTR
                                       end (* then *) ;
                                     if ( LATTR . TYPTR = REALPTR ) and
                                     ( GATTR . TYPTR = REALPTR ) then
                                       GEN0 ( 7 )
                                     else
                                       begin
                                         ERROR ( 134 ) ;
                                         GATTR . TYPTR := NIL
                                       end (* else *)
                                   end (* tag/ca *) ;
                            IDIV : if ( LATTR . TYPTR = INTPTR ) and (
                                   GATTR . TYPTR = INTPTR ) then
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
                                    ( GATTR . TYPTR = BOOLPTR ) or (
                                    LATTR . TYPTR = INTPTR ) and (
                                    GATTR . TYPTR = INTPTR ) then
                                      GEN0 ( 4 )
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
                       GEN0 ( 17 )
                     else
                       if GATTR . TYPTR = REALPTR then
                         GEN0 ( 18 )
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
                         PLUS : if ( LATTR . TYPTR = INTPTR ) and (
                                GATTR . TYPTR = INTPTR ) then
                                  GEN0 ( 2 )
                                else
                                  begin
                                    if LATTR . TYPTR = INTPTR then
                                      begin
                                        GEN0 ( 9 ) ;
                                        LATTR . TYPTR := REALPTR
                                      end (* then *)
                                    else
                                      if GATTR . TYPTR = INTPTR then
                                        begin
                                          GEN0 ( 10 ) ;
                                          GATTR . TYPTR := REALPTR
                                        end (* then *) ;
                                    if ( LATTR . TYPTR = REALPTR ) and
                                    ( GATTR . TYPTR = REALPTR ) then
                                      GEN0 ( 3 )
                                    else
                                      if ( LATTR . TYPTR -> . FORM =
                                      POWER ) and COMPTYPES ( LATTR .
                                      TYPTR , GATTR . TYPTR ) then
                                        GEN0 ( 28 )
                                      else
                                        begin
                                          ERROR ( 134 ) ;
                                          GATTR . TYPTR := NIL
                                        end (* else *)
                                  end (* else *) ;
                         MINUS : if ( LATTR . TYPTR = INTPTR ) and (
                                 GATTR . TYPTR = INTPTR ) then
                                   GEN0 ( 21 )
                                 else
                                   begin
                                     if LATTR . TYPTR = INTPTR then
                                       begin
                                         GEN0 ( 9 ) ;
                                         LATTR . TYPTR := REALPTR
                                       end (* then *)
                                     else
                                       if GATTR . TYPTR = INTPTR then
                                         begin
                                           GEN0 ( 10 ) ;
                                           GATTR . TYPTR := REALPTR
                                         end (* then *) ;
                                     if ( LATTR . TYPTR = REALPTR ) and
                                     ( GATTR . TYPTR = REALPTR ) then
                                       GEN0 ( 22 )
                                     else
                                       if ( LATTR . TYPTR -> . FORM =
                                       POWER ) and COMPTYPES ( LATTR .
                                       TYPTR , GATTR . TYPTR ) then
                                         GEN0 ( 5 )
                                       else
                                         begin
                                           ERROR ( 134 ) ;
                                           GATTR . TYPTR := NIL
                                         end (* else *)
                                   end (* else *) ;
                         OROP : if ( LATTR . TYPTR = BOOLPTR ) and (
                                GATTR . TYPTR = BOOLPTR ) or ( LATTR .
                                TYPTR = INTPTR ) and ( GATTR . TYPTR =
                                INTPTR ) then
                                  GEN0 ( 13 )
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
                      GEN0T ( 58 , GATTR . TYPTR ) ;
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
                          if LATTR . TYPTR = INTPTR then
                            begin
                              GEN0 ( 9 ) ;
                              LATTR . TYPTR := REALPTR
                            end (* then *)
                          else
                            if GATTR . TYPTR = INTPTR then
                              begin
                                GEN0 ( 10 ) ;
                                GATTR . TYPTR := REALPTR
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
                                        TYPIND := 'S'
                                      end (* tag/ca *) ;
                              ARRAYS :
                                begin
                                  if not ISSTRING ( LATTR . TYPTR ) and
                                  ( LOP in [ LTOP , LEOP , GTOP , GEOP
                                  ] ) then
                                    ERROR ( 131 ) ;
                                  if LATTR . TYPTR -> . VARY and GATTR
                                  . TYPTR -> . VARY then
                                    begin
                                      TYPIND := 'V' ;
                                      if GATTR . TYPTR -> . SIZE <
                                      LSIZE then
                                        LSIZE := GATTR . TYPTR -> .
                                                 SIZE ;
                                    end (* then *)
                                  else
                                    begin
                                      TYPIND := 'M' ;
                                      GSIZE := GATTR . TYPTR -> . SIZE
                                               ;
                                      if LATTR . TYPTR -> . VARY then
                                        LSIZE := PRED ( LSIZE ) ;
                                      if GATTR . TYPTR -> . VARY then
                                        GSIZE := PRED ( GSIZE ) ;
                                      if LSIZE <> GSIZE then
                                        ERROR ( 129 ) ;
                                    end (* else *) ;
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
                            case LOP of
                              LTOP : GEN2 ( 53 , ORD ( TYPIND ) , LSIZE
                                            ) ;
                              LEOP : GEN2 ( 52 , ORD ( TYPIND ) , LSIZE
                                            ) ;
                              GTOP : GEN2 ( 49 , ORD ( TYPIND ) , LSIZE
                                            ) ;
                              GEOP : GEN2 ( 48 , ORD ( TYPIND ) , LSIZE
                                            ) ;
                              NEOP : GEN2 ( 55 , ORD ( TYPIND ) , LSIZE
                                            ) ;
                              EQOP : GEN2 ( 47 , ORD ( TYPIND ) , LSIZE
                                            )
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
                N : INTEGER ;

            begin (* ASSIGNMENT *)
              SELECTOR ( FSYS + [ BECOMES ] , FCP ) ;
              GATTR . ACP := FCP ;
              if SY = BECOMES then
                begin
                  if GATTR . TYPTR <> NIL then
                    if ( GATTR . ACCESS <> DRCT ) or ( GATTR . TYPTR ->
                    . FORM > POWER ) then
                      LOADADDRESS ;
                  LATTR := GATTR ;
                  EXPECTSTRING := ( LATTR . TYPTR -> . FORM = ARRAYS )
                                  ;
                  INSYMBOL ;
                  EXPRESSION ( FSYS ) ;
                  EXPECTSTRING := FALSE ;
                  if GATTR . TYPTR <> NIL then
                    if GATTR . TYPTR -> . FORM <= POWER then
                      LOAD
                    else
                      LOADADDRESS ;
                  if ( LATTR . TYPTR <> NIL ) and ( GATTR . TYPTR <>
                  NIL ) then
                    begin
                      if GATTR . TYPTR = INTPTR then
                        if REALPTR = LATTR . TYPTR then
                          begin
                            GEN0 ( 10 ) ;
                            GATTR . TYPTR := REALPTR
                          end (* then *) ;
                      if COMPTYPES ( LATTR . TYPTR , GATTR . TYPTR )
                      then
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
                                GEN2T ( 45 , 0 , MAXADDR , NILPTR ) ;
                              STORE ( LATTR )
                            end (* tag/ca *) ;
                          POWER : STORE ( LATTR ) ;
                          ARRAYS :
                            begin
                              if LATTR . TYPTR -> . VARY then
                                begin
                                  if GATTR . TYPTR -> . VARY then
                                    N := LATTR . TYPTR -> . SIZE
                                  else
                                    N := SUCC ( GATTR . TYPTR -> . SIZE
                                         ) ;
                                  WRITELN ( PRR , ' MOVV ' , N : 1 ) ;
                                end (* then *)
                              else
                                if GATTR . TYPTR -> . VARY then
                                  WRITELN ( PRR , ' MVMV ' , LATTR .
                                            TYPTR -> . SIZE : 1 )
                                else
                                  GEN3 ( 40 , LATTR . TYPTR -> . SIZE ,
                                         0 , 0 ) ;
                            end (* tag/ca *) ;
                          RECORDS :
                            GEN3 ( 40 , LATTR . TYPTR -> . SIZE , 0 , 0
                                   ) ;
                          FILES : ERROR ( 146 )
                        end (* case *)
                      else
                        ERROR ( 129 )
                    end (* then *)
                end (* then *)
              else
                ERROR ( 51 )
            end (* ASSIGNMENT *) ;


         procedure GOTOSTATEMENT ;

            var LLP : LBP ;
                FOUND : BOOLEAN ;
                TTOP , TTOP1 : DISPRANGE ;

            begin (* GOTOSTATEMENT *)
              if SY = INTCONST then
                begin
                  FOUND := FALSE ;
                  TTOP := TOP ;
                  repeat
                    while DISPLAY [ TTOP ] . OCCUR <> BLCK do
                      TTOP := TTOP - 1 ;
                    TTOP1 := TTOP ;
                    LLP := DISPLAY [ TTOP ] . FLABEL ;
                    while ( LLP <> NIL ) and not FOUND do
                      with LLP -> do
                        if LABVAL = VAL . IVAL then
                          begin
                            FOUND := TRUE ;
                            if TTOP = TTOP1 then
                              GENUJP ( 57 , LABNAME )
                            else
                              ERROR ( 399 )
                          end (* then *)
                        else
                          LLP := NEXTLAB ;
                    TTOP := TTOP - 1
                  until FOUND or ( TTOP = 0 ) ;
                  if not FOUND then
                    ERROR ( 167 ) ;
                  INSYMBOL
                end (* then *)
              else
                ERROR ( 15 )
            end (* GOTOSTATEMENT *) ;


         procedure COMPOUNDSTATEMENT ;

            begin (* COMPOUNDSTATEMENT *)
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
                ERROR ( 13 )
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
              STATEMENT ( FSYS + [ ELSESY ] ) ;
              if SY = ELSESY then
                begin
                  GENLABEL ( LCIX2 ) ;
                  GENUJP ( 57 , LCIX2 ) ;
                  PUTLABEL ( LCIX1 ) ;
                  INSYMBOL ;
                  STATEMENT ( FSYS ) ;
                  PUTLABEL ( LCIX2 )
                end (* then *)
              else
                PUTLABEL ( LCIX1 )
            end (* IFSTATEMENT *) ;


         procedure CASESTATEMENT ;

            label 1 ;

            type CIP = -> CASEINFO ;
                 CASEINFO = packed record
                                     NEXT : CIP ;
                                     CSSTART : INTEGER ;
                                     CSLAB : INTEGER
                                   end ;

            var LSP , LSP1 : STP ;
                FSTPTR , LPT1 , LPT2 , LPT3 : CIP ;
                LVAL : VALU ;
                LADDR , LCIX , LCIX1 , LOTHERW , LCIXTABL , LMIN , LMAX
                : INTEGER ;
                OTHERWOCCURED : BOOLEAN ;

            begin (* CASESTATEMENT *)
              EXPRESSION ( FSYS + [ OFSY , COMMA , COLON ] ) ;
              LOAD ;
              GENLABEL ( LCIX ) ;
              OTHERWOCCURED := FALSE ;
              LSP := GATTR . TYPTR ;
              if LSP <> NIL then
                if ( LSP -> . FORM <> SCALAR ) or ( LSP = REALPTR )
                then
                  begin
                    ERROR ( 144 ) ;
                    LSP := NIL
                  end (* then *)
                else
                  if LSP <> INTPTR then
                    GEN0T ( 58 , LSP ) ;
              GENUJP ( 57 , LCIX ) ;
              if SY = OFSY then
                INSYMBOL
              else
                ERROR ( 8 ) ;
              FSTPTR := NIL ;
              GENLABEL ( LADDR ) ;
              repeat
                LPT3 := NIL ;
                if not ( SY in [ SEMICOLON , ENDSY ] ) then
                  begin
                    if SY <> OTHERWSY then
                      begin
                        GENLABEL ( LCIX1 ) ;
                        repeat
                          CONSTANT ( FSYS + [ COMMA , COLON ] , LSP1 ,
                                     LVAL ) ;
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
                                NEW ( LPT3 ) ;
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
                      end (* then *)
                    else
                      begin
                        if OTHERWOCCURED then
                          ERROR ( 156 )
                        else
                          GENLABEL ( LOTHERW ) ;
                        LCIX1 := LOTHERW ;
                        OTHERWOCCURED := TRUE ;
                        NEW ( LPT3 ) ;
                        INSYMBOL ;
                        if SY = COLON then
                          INSYMBOL ;
                      end (* else *) ;
                    PUTLABEL ( LCIX1 ) ;
                    repeat
                      STATEMENT ( FSYS + [ SEMICOLON ] )
                    until not ( SY in STATBEGSYS ) ;
                    if LPT3 <> NIL then
                      GENUJP ( 57 , LADDR ) ;
                  end (* then *) ;
                TEST := SY <> SEMICOLON ;
                if not TEST then
                  INSYMBOL
              until TEST ;
              PUTLABEL ( LCIX ) ;
              if FSTPTR <> NIL then
                begin
                  LMAX := FSTPTR -> . CSLAB ;
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
                      GEN2 ( 51 , 1 , LMIN ) ;
                      GEN0 ( 21 ) ;
                      GENLABEL ( LCIXTABL ) ;
                      GENXJP ( 44 , LMAX - LMIN , LCIXTABL ) ;
                      PUTLABEL ( LCIXTABL ) ;
                      if OTHERWOCCURED then
                        LCIX := LOTHERW
                      else
                        GENLABEL ( LCIX ) ;
                      repeat
                        with FSTPTR -> do
                          begin
                            while CSLAB > LMIN do
                              begin
                                GENJPA ( LCIXTABL , LCIX ) ;
                                LMIN := LMIN + 1
                              end (* while *) ;
                            GENJPA ( LCIXTABL , CSSTART ) ;
                            FSTPTR := NEXT ;
                            LMIN := LMIN + 1
                          end (* with *)
                      until FSTPTR = NIL ;
                      if not OTHERWOCCURED then
                        begin
                          PUTLABEL ( LCIX ) ;
                          if DEBUG then
                            GEN1 ( 60 , 1 )
                        end (* then *)
                      else
                        GENUJP ( 57 , LOTHERW ) ;
                      PUTLABEL ( LADDR )
                    end (* then *)
                  else
                    ERROR ( 157 )
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
                STATEMENT ( FSYS + [ SEMICOLON , UNTILSY ] ) ;
                if SY in STATBEGSYS then
                  ERROR ( 14 )
              until not ( SY in STATBEGSYS ) ;
              while SY = SEMICOLON do
                begin
                  INSYMBOL ;
                  repeat
                    STATEMENT ( FSYS + [ SEMICOLON , UNTILSY ] ) ;
                    if SY in STATBEGSYS then
                      ERROR ( 14 )
                  until not ( SY in STATBEGSYS ) ;
                end (* while *) ;
              if SY = UNTILSY then
                begin
                  INSYMBOL ;
                  EXPRESSION ( FSYS ) ;
                  GENFJP ( LADDR )
                end (* then *)
              else
                ERROR ( 53 )
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
              STATEMENT ( FSYS ) ;
              GENUJP ( 57 , LADDR ) ;
              PUTLABEL ( LCIX )
            end (* WHILESTATEMENT *) ;


         procedure FORSTATEMENT ;

            var LATTR : ATTR ;
                LSP : STP ;
                LSY : SYMBOL ;
                LCIX , LADDR : INTEGER ;
                LLC , LCLIM : ADDRRANGE ;

            begin (* FORSTATEMENT *)
              LLC := LC ;
              with LATTR do
                begin
                  TYPTR := NIL ;
                  KIND := VARBL ;
                  ACCESS := DRCT ;
                  VLEVEL := LEVEL ;
                  DPLMT := 0 ;
                  LABEL_ACCESS := FALSE ;
                  ACP := NIL ;
                end (* with *) ;
              if SY = IDENT then
                begin
                  SEARCHID ( [ VARS ] , LCP ) ;
                  with LCP -> , LATTR do
                    begin
                      TYPTR := IDTYPE ;
                      KIND := VARBL ;
                      ACP := LCP ;
                      if VKIND = ACTUAL then
                        begin
                          ACCESS := DRCT ;
                          VLEVEL := VLEV ;
                          DPLMT := VADDR
                        end (* then *)
                      else
                        begin
                          ERROR ( 155 ) ;
                          TYPTR := NIL
                        end (* else *)
                    end (* with *) ;
                  if LATTR . TYPTR <> NIL then
                    if ( LATTR . TYPTR -> . FORM > SUBRANGE ) or (
                    REALPTR = LATTR . TYPTR ) then
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
                          GEN0T ( 63 , LATTR . TYPTR ) ;
                          STORE ( LATTR )
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
                          if LATTR . TYPTR <> INTPTR then
                            GEN0T ( 58 , GATTR . TYPTR ) ;
                          if FPROCP <> NIL then
                            begin
                              LC := LC - INTSIZE ;
                              ALIGN ( NILPTR , LC ) ;
                              LCLIM := LC ;
                              if LC < LCMAX then
                                LCMAX := LC
                            end (* then *)
                          else
                            begin
                              ALIGN ( NILPTR , LC ) ;
                              LCLIM := LC ;
                              LC := LC + INTSIZE ;
                              if LC > LCMAX then
                                LCMAX := LC ;
                            end (* else *) ;
                          GENLABEL ( LCIX ) ;
                          if LSY = TOSY then
                            GEN2TL ( 64 , LCLIM , LCIX , LATTR . TYPTR
                                     )
                          else
                            GEN2TL ( 66 , LCLIM , LCIX , LATTR . TYPTR
                                     ) ;
                          PPUTLC ;
                          GENLABEL ( LADDR ) ;
                          PUTLABEL ( LADDR ) ;
                        end (* then *)
                      else
                        ERROR ( 145 )
                end (* then *)
              else
                begin
                  ERROR ( 55 ) ;
                  SKIP ( FSYS + [ DOSY ] )
                end (* else *) ;
              if SY = DOSY then
                INSYMBOL
              else
                ERROR ( 54 ) ;
              STATEMENT ( FSYS ) ;
              GATTR := LATTR ;
              LOADADDRESS ;
              if LSY = TOSY then
                GEN2TL ( 65 , LCLIM , LADDR , LATTR . TYPTR )
              else
                GEN2TL ( 67 , LCLIM , LADDR , LATTR . TYPTR ) ;
              PUTLABEL ( LCIX ) ;
              LC := LLC ;
            end (* FORSTATEMENT *) ;


         procedure WITHSTATEMENT ;

            var LCP : CTP ;
                LCNT1 : DISPRANGE ;
                LLC : ADDRRANGE ;

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
                            FLABEL := NIL
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
                            if FPROCP <> NIL then
                              begin
                                LC := LC - PTRSIZE ;
                                ALIGN ( NILPTR , LC ) ;
                                if LC < LCMAX then
                                  LCMAX := LC
                              end (* then *)
                            else
                              ALIGN ( NILPTR , LC ) ;
                            GEN2T ( 56 , 0 , LC , NILPTR ) ;
                            with DISPLAY [ TOP ] do
                              begin
                                OCCUR := VREC ;
                                VDSPL := LC
                              end (* with *) ;
                            if FPROCP = NIL then
                              begin
                                LC := LC + PTRSIZE ;
                                if LC > LCMAX then
                                  LCMAX := LC
                              end (* then *) ;
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
              STATEMENT ( FSYS ) ;
              TOP := TOP - LCNT1 ;
              LC := LLC ;
            end (* WITHSTATEMENT *) ;


         begin (* STATEMENT *)
           STNEST := SUCC ( STNEST ) ;
           if SY = INTCONST then
             begin
               LLP := DISPLAY [ LABELSDISP ] . FLABEL ;
               while LLP <> NIL do
                 with LLP -> do
                   if LABVAL = VAL . IVAL then
                     begin
                       if DEFINED then
                         ERROR ( 165 ) ;
                       PUTLABEL ( LABNAME ) ;
                       DEFINED := TRUE ;
                       PPUTLC ;
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
             end (* then *) ;
           STNEST := PRED ( STNEST ) ;
         end (* STATEMENT *) ;


      begin (* BODY *)
        CSTPTRIX := 0 ;
        TOPNEW := LCAFTERMARKSTACK ;
        TOPMAX := LCAFTERMARKSTACK ;
        if FPROCP = NIL then
          begin
            WRITELN ( PRR , '__MAIN' ) ;
            WRITELN ( PRR , ' PID''MAIN''' ) ;
            PPUTLC ;
            WRITELN ( PRR , ' ENT -1 0' ) ;
            LLCP := FILECP ;
            while LLCP <> NIL do
              begin
                GEN2 ( 51 , 1 , 0 ) ;
                GEN1T ( 43 , LLCP -> . VADDR + 4 , INTPTR ) ;
                LLCP := LLCP -> . NEXT ;
              end (* while *) ;
            GEN1 ( 37 , FILECP -> . VADDR ) ;
            GEN2 ( 51 , 1 , 0 ) ;
            WRITELN ( PRR , ' LDCI      1  ; output' ) ;
            GEN2 ( 51 , 1 , 0 ) ;
            GEN1 ( 30 , 25 ) ;
            GEN1 ( 37 , FILECP -> . NEXT -> . VADDR ) ;
            GEN2 ( 51 , 1 , 0 ) ;
            WRITELN ( PRR , ' LDCI      0  ; input' ) ;
            GEN2 ( 51 , 1 , 0 ) ;
            GEN1 ( 30 , 24 ) ;
          end (* then *)
        else
          begin
            PUTLABEL ( FPROCP -> . PFNAME ) ;
            if ( FPROCP -> . PF_ATTR = EXPRT ) and PRCODE then
              WRITENAME ( FPROCP -> . NAME ) ;
            WRITE ( PRR , ' PID''' ) ;
            WRITENAME ( FPROCP -> . NAME ) ;
            WRITELN ( PRR , '''' ) ;
            PPUTLC ;
            GENLABEL ( SEGSIZE ) ;
            GENLABEL ( STACKTOP ) ;
            GENCUPENT ( 32 , 1 , SEGSIZE ) ;
            if DEBUG then
              GENCUPENT ( 32 , 2 , STACKTOP ) ;
            LLC1 := LCBEFOREMARKSTACK + FPROCP -> . NUMB_OF_PARM *
                    PTRSIZE ;
            LCP := FPROCP -> . NEXT ;
            while LCP <> NIL do
              with LCP -> do
                begin
                  if IDTYPE -> . FORM = POWER then
                    LLC1 := LLC1 - SETSIZE
                  else
                    LLC1 := LLC1 - PTRSIZE ;
                  ALIGN ( PARMPTR , LLC1 ) ;
                  if KLASS = VARS then
                    if IDTYPE <> NIL then
                      if IDTYPE -> . FORM > POWER then
                        begin
                          if VKIND = ACTUAL then
                            begin
                              GEN2 ( 50 , 0 , VADDR ) ;
                              GEN2T ( 54 , 0 , LLC1 , NILPTR ) ;
                              GEN3 ( 40 , IDTYPE -> . SIZE , 0 , 0 ) ;
                            end (* then *) ;
                        end (* then *) ;
                  LCP := LCP -> . NEXT ;
                end (* with *) ;
          end (* else *) ;
        LCMAX := LC ;
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
        LLP := DISPLAY [ TOP ] . FLABEL ;
        while LLP <> NIL do
          with LLP -> do
            begin
              if not DEFINED then
                begin
                  ERROR ( 168 ) ;
                  WRITELN ( OUTPUT ) ;
                  WRITELN ( OUTPUT , ' LABEL ' , LABVAL ) ;
                end (* then *) ;
              LLP := NEXTLAB
            end (* with *) ;
        if FPROCP <> NIL then
          begin
            ALIGN ( PARMPTR , LCMAX ) ;
            if FPROCP -> . IDTYPE = NIL then
              GEN1 ( 42 , ORD ( 'P' ) )
            else
              with FPROCP -> do
                begin
                  GEN0T ( 42 , IDTYPE ) ;
                  WRITE ( PRR , '&' , RESULT_LABEL : 1 , '= ' ) ;
                  if ( IDTYPE = BOOLPTR ) or ( IDTYPE = CHARPTR ) then
                    WRITELN ( PRR , LCMAX - 1 )
                  else
                    WRITELN ( PRR , LCMAX - 4 ) ;
                end (* with *) ;
            if PRCODE then
              begin
                WRITELN ( PRR , '&' , SEGSIZE : 1 , '= ' , LCMAX ) ;
                WRITELN ( PRR , '&' , STACKTOP : 1 , '= ' , TOPMAX )
              end (* then *)
          end (* then *)
        else
          begin
            GEN1 ( 60 , 0 ) ;
            if PRCODE then
              WRITELN ( PRR , ' ENT 0 ' , LCMAX : 6 ) ;
            SAVEID := ID ;
            while FEXTFILEP <> NIL do
              begin
                with FEXTFILEP -> do
                  if not ( ( FILENAME = 'INPUT           ' ) or (
                  FILENAME = 'OUTPUT          ' ) ) then
                    begin
                      ID := FILENAME ;
                      SEARCHID ( [ VARS ] , LLCP ) ;
                      if LLCP -> . IDTYPE <> NIL then
                        if LLCP -> . IDTYPE -> . FORM <> FILES then
                          begin
                            WRITELN ( OUTPUT ) ;
                            WRITELN ( OUTPUT , ' ' : 8 , 'UNDECLARED '
                                      , 'EXTERNAL ' , 'FILE' ,
                                      FEXTFILEP -> . FILENAME : 8 ) ;
                            WRITE ( OUTPUT , ' ' : CHCNT + 16 )
                          end (* then *)
                    end (* then *) ;
                FEXTFILEP := FEXTFILEP -> . NEXTFILE
              end (* while *) ;
            ID := SAVEID ;
          end (* else *) ;
      end (* BODY *) ;


   begin (* BLOCK *)
     LABELSDISP := TOP ;
     TEST := FALSE ;
     if FPROCP <> NIL then
       if FPROCP -> . PF_ATTR = IMPRT then
         begin
           WRITELN ;
           WRITELN ( 'block wurde bei importierter proc aufgerufen' ) ;
           BODY ( FSYS ) ;
           TEST := TRUE ;
         end (* then *) ;
     if not TEST then
       begin
         DP := TRUE ;
         while SY in [ LABELSY , CONSTSY , TYPESY , VARSY , PROCSY ,
         FUNCSY ] do
           case SY of
             LABELSY :
               begin
                 INSYMBOL ;
                 LABELDECLARATION
               end (* tag/ca *) ;
             CONSTSY :
               begin
                 INSYMBOL ;
                 CONSTDECLARATION
               end (* tag/ca *) ;
             TYPESY :
               begin
                 INSYMBOL ;
                 TYPEDECLARATION
               end (* tag/ca *) ;
             VARSY : begin
                       INSYMBOL ;
                       VARDECLARATION
                     end (* tag/ca *) ;
             PROCSY , FUNCSY :
               begin
                 LSY := SY ;
                 INSYMBOL ;
                 PROCDECLARATION ( LSY )
               end (* tag/ca *) ;
             otherwise
               
             : if SY <> BEGINSY then
                 begin
                   ERROR ( 18 ) ;
                   SKIP ( FSYS )
                 end (* then *)
           end (* case *) ;
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
         until ( ( SY = FSY ) or ( SY in BLOCKBEGSYS ) ) or EOF ( INPUT
         ) ;
       end (* then *) ;
   end (* BLOCK *) ;



procedure PROGRAMME ( FSYS : SETOFSYS ) ;

   var EXTFP : EXTFILEP ;

   begin (* PROGRAMME *)
     if SY = PROGSY then
       begin
         INSYMBOL ;
         if SY <> IDENT then
           ERROR ( 2 )
         else
           WRITELN ( PRR , '; PROG ' , ID ) ;
         INSYMBOL ;
         if not ( SY in [ LPARENT , SEMICOLON ] ) then
           ERROR ( 14 ) ;
         if SY = LPARENT then
           begin
             repeat
               INSYMBOL ;
               if SY = IDENT then
                 begin
                   NEW ( EXTFP ) ;
                   with EXTFP -> do
                     begin
                       FILENAME := ID ;
                       NEXTFILE := FEXTFILEP
                     end (* with *) ;
                   FEXTFILEP := EXTFP ;
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
       end (* then *) ;
     repeat
       BLOCK ( FSYS , PERIOD , NIL ) ;
       if SY <> PERIOD then
         ERROR ( 21 )
     until ( SY = PERIOD ) or EOF ( INPUT ) ;
     if ERRINX <> 0 then
       INSYMBOL
   end (* PROGRAMME *) ;



procedure STDNAMES ;

   begin (* STDNAMES *)
     NA [ 1 ] := 'FALSE' ;
     NA [ 2 ] := 'TRUE' ;
     NA [ 3 ] := 'INPUT' ;
     NA [ 4 ] := 'OUTPUT' ;
     NA [ 5 ] := 'GET' ;
     NA [ 6 ] := 'PUT' ;
     NA [ 7 ] := 'RESET' ;
     NA [ 8 ] := 'REWRITE' ;
     NA [ 9 ] := 'READ' ;
     NA [ 10 ] := 'WRITE' ;
     NA [ 11 ] := 'PACK' ;
     NA [ 12 ] := 'UNPACK' ;
     NA [ 13 ] := 'NEW' ;
     NA [ 14 ] := 'RELEASE' ;
     NA [ 15 ] := 'READLN' ;
     NA [ 16 ] := 'WRITELN' ;
     NA [ 17 ] := 'ABS' ;
     NA [ 18 ] := 'SQR' ;
     NA [ 19 ] := 'TRUNC' ;
     NA [ 20 ] := 'ODD' ;
     NA [ 21 ] := 'ORD' ;
     NA [ 22 ] := 'CHR' ;
     NA [ 23 ] := 'PRED' ;
     NA [ 24 ] := 'SUCC' ;
     NA [ 25 ] := 'EOF' ;
     NA [ 26 ] := 'EOLN' ;
     NA [ 27 ] := 'SIN' ;
     NA [ 28 ] := 'COS' ;
     NA [ 29 ] := 'EXP' ;
     NA [ 30 ] := 'SQRT' ;
     NA [ 31 ] := 'LN' ;
     NA [ 32 ] := 'ARCTAN' ;
     NA [ 33 ] := 'ROUND' ;
     NA [ 34 ] := '//' ;
     NA [ 35 ] := 'MARK' ;
     NA [ 36 ] := 'SIZEOF' ;
     NA [ 37 ] := 'ADDROF' ;
   end (* STDNAMES *) ;



procedure ENTERSTDTYPES ;

   begin (* ENTERSTDTYPES *)
     NEW ( INTPTR , SCALAR , STANDARD ) ;
     with INTPTR -> do
       begin
         SIZE := INTSIZE ;
         FORM := SCALAR ;
         SCALKIND := STANDARD
       end (* with *) ;
     NEW ( REALPTR , SCALAR , STANDARD ) ;
     with REALPTR -> do
       begin
         SIZE := REALSIZE ;
         FORM := SCALAR ;
         SCALKIND := STANDARD
       end (* with *) ;
     NEW ( CHARPTR , SCALAR , STANDARD ) ;
     with CHARPTR -> do
       begin
         SIZE := CHARSIZE ;
         FORM := SCALAR ;
         SCALKIND := STANDARD
       end (* with *) ;
     NEW ( BOOLPTR , SCALAR , DECLARED ) ;
     with BOOLPTR -> do
       begin
         SIZE := BOOLSIZE ;
         FORM := SCALAR ;
         SCALKIND := DECLARED
       end (* with *) ;
     NEW ( NILPTR , POINTER ) ;
     with NILPTR -> do
       begin
         ELTYPE := NIL ;
         SIZE := PTRSIZE ;
         FORM := POINTER
       end (* with *) ;
     NEW ( PARMPTR , SCALAR , STANDARD ) ;
     with PARMPTR -> do
       begin
         SIZE := PARMSIZE ;
         FORM := SCALAR ;
         SCALKIND := STANDARD
       end (* with *) ;
     NEW ( TEXTPTR , FILES ) ;
     with TEXTPTR -> do
       begin
         FILTYPE := CHARPTR ;
         SIZE := CHARSIZE + FCBSIZE ;
         FORM := FILES
       end (* with *)
   end (* ENTERSTDTYPES *) ;



procedure ENTSTDNAMES ;

   var CP , CP1 : CTP ;
       I : INTEGER ;
       SP , SP1 : STP ;

   begin (* ENTSTDNAMES *)
     NEW ( CP , TYPES ) ;
     with CP -> do
       begin
         NAME := 'INTEGER' ;
         IDTYPE := INTPTR ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;
     NEW ( CP , TYPES ) ;
     with CP -> do
       begin
         NAME := 'REAL' ;
         IDTYPE := REALPTR ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;
     NEW ( CP , TYPES ) ;
     with CP -> do
       begin
         NAME := 'CHAR' ;
         IDTYPE := CHARPTR ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;
     NEW ( CP , TYPES ) ;
     with CP -> do
       begin
         NAME := 'BOOLEAN' ;
         IDTYPE := BOOLPTR ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;
     NEW ( CP , TYPES ) ;
     with CP -> do
       begin
         NAME := 'TEXT' ;
         IDTYPE := TEXTPTR ;
         KLASS := TYPES
       end (* with *) ;
     ENTERID ( CP ) ;
     CP1 := NIL ;
     for I := 1 to 2 do
       begin
         NEW ( CP , KONST ) ;
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
     with CP -> do
       begin
         NAME := 'NIL' ;
         IDTYPE := NILPTR ;
         NEXT := NIL ;
         VALUES . IVAL := 0 ;
         KLASS := KONST
       end (* with *) ;
     ENTERID ( CP ) ;
     CP1 := NIL ;
     for I := 3 to 4 do
       begin
         NEW ( CP , VARS ) ;
         with CP -> do
           begin
             NAME := NA [ I ] ;
             IDTYPE := TEXTPTR ;
             KLASS := VARS ;
             VKIND := ACTUAL ;
             NEXT := CP1 ;
             CP1 := CP ;
             VLEV := 1 ;
             VXNAME := NIL ;
             VIMPORT := FALSE ;
             VADDR := INTSIZE + PTRSIZE + ( I - 3 ) * ( FCBSIZE +
                      CHARSIZE + 1 ) ;
           end (* with *) ;
         ENTERID ( CP )
       end (* for *) ;
     FILECP := CP ;
     NEW ( CP , VARS ) ;
     with CP -> do
       begin
         NAME := 'ARGC' ;
         IDTYPE := INTPTR ;
         KLASS := VARS ;
         VKIND := ACTUAL ;
         NEXT := NIL ;
         VLEV := 1 ;
         VADDR := 0 ;
         VXNAME := NIL ;
         VIMPORT := FALSE ;
       end (* with *) ;
     ENTERID ( CP ) ;
     NEW ( SP1 , SUBRANGE ) ;
     with SP1 -> do
       begin
         SIZE := INTSIZE ;
         FORM := SUBRANGE ;
         RANGETYPE := INTPTR ;
         MIN . IVAL := 1 ;
         MAX . IVAL := 256 ;
       end (* with *) ;
     NEW ( SP , ARRAYS ) ;
     with SP -> do
       begin
         SIZE := 256 * CHARSIZE ;
         FORM := ARRAYS ;
         AELTYPE := CHARPTR ;
         INXTYPE := SP1 ;
         VARY := TRUE ;
       end (* with *) ;
     NEW ( SP1 , POINTER ) ;
     with SP1 -> do
       begin
         SIZE := PTRSIZE ;
         FORM := POINTER ;
         ELTYPE := SP ;
       end (* with *) ;
     NEW ( SP , ARRAYS ) ;
     with SP -> do
       begin
         SIZE := 1001 * PTRSIZE ;
         FORM := ARRAYS ;
         AELTYPE := SP1 ;
         VARY := FALSE ;
       end (* with *) ;
     NEW ( SP1 , SUBRANGE ) ;
     with SP1 -> do
       begin
         SIZE := INTSIZE ;
         FORM := SUBRANGE ;
         RANGETYPE := INTPTR ;
         MIN . IVAL := 0 ;
         MAX . IVAL := 1000 ;
       end (* with *) ;
     SP -> . INXTYPE := SP1 ;
     NEW ( SP1 , POINTER ) ;
     with SP1 -> do
       begin
         SIZE := PTRSIZE ;
         FORM := POINTER ;
         ELTYPE := SP ;
       end (* with *) ;
     NEW ( CP , VARS ) ;
     with CP -> do
       begin
         NAME := 'ARGV' ;
         IDTYPE := SP1 ;
         KLASS := VARS ;
         VKIND := ACTUAL ;
         NEXT := NIL ;
         VLEV := 1 ;
         VADDR := INTSIZE ;
         VXNAME := NIL ;
         VIMPORT := FALSE ;
       end (* with *) ;
     ENTERID ( CP ) ;
     for I := 5 to 16 do
       begin
         NEW ( CP , PROC , STANDARD ) ;
         with CP -> do
           begin
             NAME := NA [ I ] ;
             IDTYPE := NIL ;
             NEXT := NIL ;
             KEY := I - 4 ;
             KLASS := PROC ;
             PFDECKIND := STANDARD
           end (* with *) ;
         ENTERID ( CP )
       end (* for *) ;
     NEW ( CP , PROC , STANDARD ) ;
     with CP -> do
       begin
         NAME := NA [ 35 ] ;
         IDTYPE := NIL ;
         NEXT := NIL ;
         KEY := 13 ;
         KLASS := PROC ;
         PFDECKIND := STANDARD
       end (* with *) ;
     ENTERID ( CP ) ;
     for I := 17 to 26 do
       begin
         NEW ( CP , FUNC , STANDARD ) ;
         with CP -> do
           begin
             NAME := NA [ I ] ;
             IDTYPE := NIL ;
             NEXT := NIL ;
             KEY := I - 16 ;
             KLASS := FUNC ;
             PFDECKIND := STANDARD
           end (* with *) ;
         ENTERID ( CP )
       end (* for *) ;
     NEW ( CP , FUNC , STANDARD ) ;
     with CP -> do
       begin
         NAME := NA [ 33 ] ;
         IDTYPE := NIL ;
         NEXT := NIL ;
         KEY := 11 ;
         KLASS := FUNC ;
         PFDECKIND := STANDARD
       end (* with *) ;
     ENTERID ( CP ) ;
     for I := 36 to 37 do
       begin
         NEW ( CP , FUNC , STANDARD ) ;
         with CP -> do
           begin
             NAME := NA [ I ] ;
             IDTYPE := NIL ;
             NEXT := NIL ;
             KEY := I - 24 ;
             KLASS := FUNC ;
             PFDECKIND := STANDARD
           end (* with *) ;
         ENTERID ( CP )
       end (* for *) ;
     NEW ( CP , VARS ) ;
     with CP -> do
       begin
         NAME := '  ' ;
         IDTYPE := REALPTR ;
         KLASS := VARS ;
         VKIND := ACTUAL ;
         NEXT := NIL ;
         VLEV := 1 ;
         VADDR := 0 ;
         VXNAME := NIL ;
         VIMPORT := FALSE ;
       end (* with *) ;
     for I := 27 to 32 do
       begin
         NEW ( CP1 , FUNC , DECLARED , ACTUAL ) ;
         with CP1 -> do
           begin
             NAME := NA [ I ] ;
             IDTYPE := REALPTR ;
             NEXT := CP ;
             FORWDECL := FALSE ;
             PF_ATTR := IMPRT ;
             PFLEV := 0 ;
             PFNAME := I - 12 ;
             PFXNAME := NIL ;
             KLASS := FUNC ;
             PFDECKIND := DECLARED ;
             PFKIND := ACTUAL
           end (* with *) ;
         ENTERID ( CP1 )
       end (* for *)
   end (* ENTSTDNAMES *) ;



procedure ENTERUNDECL ;

   var LBL : INTEGER ;

   begin (* ENTERUNDECL *)
     NEW ( UTYPPTR , TYPES ) ;
     with UTYPPTR -> do
       begin
         NAME := '  ' ;
         IDTYPE := NIL ;
         KLASS := TYPES
       end (* with *) ;
     NEW ( UCSTPTR , KONST ) ;
     with UCSTPTR -> do
       begin
         NAME := '  ' ;
         IDTYPE := NIL ;
         NEXT := NIL ;
         VALUES . IVAL := 0 ;
         KLASS := KONST
       end (* with *) ;
     NEW ( UVARPTR , VARS ) ;
     with UVARPTR -> do
       begin
         NAME := '  ' ;
         IDTYPE := NIL ;
         VKIND := ACTUAL ;
         VXNAME := NIL ;
         VIMPORT := FALSE ;
         NEXT := NIL ;
         VLEV := 0 ;
         VADDR := 0 ;
         KLASS := VARS
       end (* with *) ;
     NEW ( UFLDPTR , FIELD ) ;
     with UFLDPTR -> do
       begin
         NAME := '  ' ;
         IDTYPE := NIL ;
         NEXT := NIL ;
         FLDADDR := 0 ;
         KLASS := FIELD
       end (* with *) ;
     NEW ( UPRCPTR , PROC , DECLARED , ACTUAL ) ;
     with UPRCPTR -> do
       begin
         NAME := '  ' ;
         IDTYPE := NIL ;
         FORWDECL := FALSE ;
         NEXT := NIL ;
         PF_ATTR := IMPRT ;
         PFLEV := 0 ;
         PFXNAME := NIL ;
         GENLABEL ( LBL ) ;
         PFNAME := LBL ;
         KLASS := PROC ;
         PFDECKIND := DECLARED ;
         PFKIND := ACTUAL
       end (* with *) ;
     NEW ( UFCTPTR , FUNC , DECLARED , ACTUAL ) ;
     with UFCTPTR -> do
       begin
         NAME := '  ' ;
         IDTYPE := NIL ;
         NEXT := NIL ;
         FORWDECL := FALSE ;
         PF_ATTR := IMPRT ;
         PFLEV := 0 ;
         PFXNAME := NIL ;
         GENLABEL ( LBL ) ;
         PFNAME := LBL ;
         KLASS := FUNC ;
         PFDECKIND := DECLARED ;
         PFKIND := ACTUAL
       end (* with *)
   end (* ENTERUNDECL *) ;



procedure INITSCALARS ;

   begin (* INITSCALARS *)
     FWPTR := NIL ;
     LIST := FALSE ;
     PRCODE := TRUE ;
     DEBUG := TRUE ;
     EXPECTSTRING := FALSE ;
     DP := TRUE ;
     PRTERR := TRUE ;
     ERRINX := 0 ;
     TOTALERRS := 0 ;
     INTLABEL := 2000 ;
     KK := ALPHALEN ;
     FEXTFILEP := NIL ;
     LC := INTSIZE + PTRSIZE + FILEBUFFER * ( FCBSIZE + CHARSIZE + 1 )
           ;
     IC := 1 ;
     EOL := TRUE ;
     OLDLINECOUNT := 0 ;
     LINECOUNT := 0 ;
     CRNTLNPOS := 0 ;
     CH := ' ' ;
     CHCNT := 0 ;
     GLOBTESTP := NIL ;
     MXINT10 := MAXINT DIV 10 ;
     DIGMAX := RSTRGLGTH - 1 ;
   end (* INITSCALARS *) ;



procedure INITSETS ;

   begin (* INITSETS *)
     CONSTBEGSYS := [ ADDOP , INTCONST , REALCONST , STRINGCONST ,
                    IDENT ] ;
     SIMPTYPEBEGSYS := [ LPARENT ] + CONSTBEGSYS ;
     TYPEBEGSYS := [ ARROW , PACKEDSY , ARRAYSY , STRINGSY , RECORDSY ,
                   SETSY , FILESY ] + SIMPTYPEBEGSYS ;
     TYPEDELS := [ ARRAYSY , STRINGSY , RECORDSY , SETSY , FILESY ] ;
     BLOCKBEGSYS := [ LABELSY , CONSTSY , TYPESY , VARSY , PROCSY ,
                    FUNCSY , BEGINSY ] ;
     SELECTSYS := [ ARROW , PERIOD , LBRACK ] ;
     FACBEGSYS := [ INTCONST , REALCONST , STRINGCONST , IDENT ,
                  LPARENT , LBRACK , NOTSY ] ;
     STATBEGSYS := [ BEGINSY , GOTOSY , IFSY , WHILESY , REPEATSY ,
                   FORSY , WITHSY , CASESY ] ;
   end (* INITSETS *) ;



procedure INITTABLES ;


   procedure RESWORDS ;

      var I : INTEGER ;

      begin (* RESWORDS *)
        RW [ 1 ] := 'IF' ;
        RW [ 2 ] := 'DO' ;
        RW [ 3 ] := 'OF' ;
        RW [ 4 ] := 'TO' ;
        RW [ 5 ] := 'IN' ;
        RW [ 6 ] := 'OR' ;
        RW [ 7 ] := 'END' ;
        RW [ 8 ] := 'FOR' ;
        RW [ 9 ] := 'VAR' ;
        RW [ 10 ] := 'DIV' ;
        RW [ 11 ] := 'MOD' ;
        RW [ 12 ] := 'SET' ;
        RW [ 13 ] := 'AND' ;
        RW [ 14 ] := 'NOT' ;
        RW [ 15 ] := 'THEN' ;
        RW [ 16 ] := 'ELSE' ;
        RW [ 17 ] := 'WITH' ;
        RW [ 18 ] := 'GOTO' ;
        RW [ 19 ] := 'CASE' ;
        RW [ 20 ] := 'TYPE' ;
        RW [ 21 ] := 'FILE' ;
        RW [ 22 ] := 'BEGIN' ;
        RW [ 23 ] := 'UNTIL' ;
        RW [ 24 ] := 'WHILE' ;
        RW [ 25 ] := 'ARRAY' ;
        RW [ 26 ] := 'CONST' ;
        RW [ 27 ] := 'LABEL' ;
        RW [ 28 ] := 'REPEAT' ;
        RW [ 29 ] := 'RECORD' ;
        RW [ 30 ] := 'DOWNTO' ;
        RW [ 31 ] := 'PACKED' ;
        RW [ 32 ] := 'STRING' ;
        RW [ 33 ] := 'IMPORT' ;
        RW [ 34 ] := 'EXPORT' ;
        RW [ 35 ] := 'FORWARD' ;
        RW [ 36 ] := 'PROGRAM' ;
        RW [ 37 ] := 'FUNCTION' ;
        RW [ 38 ] := 'PROCEDURE' ;
        RW [ 39 ] := 'OTHERWISE' ;
        FRW [ 1 ] := 1 ;
        FRW [ 2 ] := 1 ;
        FRW [ 3 ] := 7 ;
        FRW [ 4 ] := 15 ;
        FRW [ 5 ] := 22 ;
        FRW [ 6 ] := 28 ;
        FRW [ 7 ] := 35 ;
        FRW [ 8 ] := 37 ;
        FRW [ 9 ] := 38 ;
        for I := 10 to SUCCALPHALEN do
          FRW [ I ] := 40 ;
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
        RSY [ 15 ] := THENSY ;
        RSY [ 16 ] := ELSESY ;
        RSY [ 17 ] := WITHSY ;
        RSY [ 18 ] := GOTOSY ;
        RSY [ 19 ] := CASESY ;
        RSY [ 20 ] := TYPESY ;
        RSY [ 21 ] := FILESY ;
        RSY [ 22 ] := BEGINSY ;
        RSY [ 23 ] := UNTILSY ;
        RSY [ 24 ] := WHILESY ;
        RSY [ 25 ] := ARRAYSY ;
        RSY [ 26 ] := CONSTSY ;
        RSY [ 27 ] := LABELSY ;
        RSY [ 28 ] := REPEATSY ;
        RSY [ 29 ] := RECORDSY ;
        RSY [ 30 ] := DOWNTOSY ;
        RSY [ 31 ] := PACKEDSY ;
        RSY [ 32 ] := STRINGSY ;
        RSY [ 33 ] := IMPORTSY ;
        RSY [ 34 ] := EXPORTSY ;
        RSY [ 35 ] := FORWARDSY ;
        RSY [ 36 ] := PROGSY ;
        RSY [ 37 ] := FUNCSY ;
        RSY [ 38 ] := PROCSY ;
        RSY [ 39 ] := OTHERWSY ;
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
        SSY [ '@' ] := ARROW ;
        SSY [ '^' ] := ARROW ;
        SSY [ '<' ] := RELOP ;
        SSY [ '>' ] := RELOP ;
        SSY [ ';' ] := SEMICOLON ;
      end (* SYMBOLS *) ;


   procedure RATORS ;

      var I : INTEGER ;
          CH : CHAR ;

      begin (* RATORS *)
        for I := 1 to 39 do
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
        SNA [ 1 ] := ' GET' ;
        SNA [ 2 ] := ' PUT' ;
        SNA [ 3 ] := ' RDI' ;
        SNA [ 4 ] := ' RDR' ;
        SNA [ 5 ] := ' RDC' ;
        SNA [ 6 ] := ' RDS' ;
        SNA [ 7 ] := ' WRI' ;
        SNA [ 8 ] := ' WRR' ;
        SNA [ 9 ] := ' WRC' ;
        SNA [ 10 ] := ' WRS' ;
        SNA [ 11 ] := ' PAK' ;
        SNA [ 12 ] := ' NEW' ;
        SNA [ 13 ] := ' RST' ;
        SNA [ 14 ] := ' ELN' ;
        SNA [ 15 ] := ' SIN' ;
        SNA [ 16 ] := ' COS' ;
        SNA [ 17 ] := ' EXP' ;
        SNA [ 18 ] := ' SQT' ;
        SNA [ 19 ] := ' LOG' ;
        SNA [ 20 ] := ' ATN' ;
        SNA [ 21 ] := ' RLN' ;
        SNA [ 22 ] := ' WLN' ;
        SNA [ 23 ] := ' SAV' ;
        SNA [ 24 ] := ' RES' ;
        SNA [ 25 ] := ' REW' ;
      end (* PROCMNEMONICS *) ;


   procedure INSTRMNEMONICS ;

      begin (* INSTRMNEMONICS *)
        MN [ 0 ] := ' ABI' ;
        MN [ 1 ] := ' ABR' ;
        MN [ 2 ] := ' ADI' ;
        MN [ 3 ] := ' ADR' ;
        MN [ 4 ] := ' AND' ;
        MN [ 5 ] := ' DIF' ;
        MN [ 6 ] := ' DVI' ;
        MN [ 7 ] := ' DVR' ;
        MN [ 8 ] := ' EOF' ;
        MN [ 9 ] := ' FLO' ;
        MN [ 10 ] := ' FLT' ;
        MN [ 11 ] := ' INN' ;
        MN [ 12 ] := ' INT' ;
        MN [ 13 ] := ' IOR' ;
        MN [ 14 ] := ' MOD' ;
        MN [ 15 ] := ' MPI' ;
        MN [ 16 ] := ' MPR' ;
        MN [ 17 ] := ' NGI' ;
        MN [ 18 ] := ' NGR' ;
        MN [ 19 ] := ' NOT' ;
        MN [ 20 ] := ' ODD' ;
        MN [ 21 ] := ' SBI' ;
        MN [ 22 ] := ' SBR' ;
        MN [ 23 ] := ' SGS' ;
        MN [ 24 ] := ' SQI' ;
        MN [ 25 ] := ' SQR' ;
        MN [ 26 ] := ' STO' ;
        MN [ 27 ] := ' TRC' ;
        MN [ 28 ] := ' UNI' ;
        MN [ 29 ] := ' STP' ;
        MN [ 30 ] := ' CSP' ;
        MN [ 31 ] := ' DEC' ;
        MN [ 32 ] := ' ENT' ;
        MN [ 33 ] := ' FJP' ;
        MN [ 34 ] := ' INC' ;
        MN [ 35 ] := ' IND' ;
        MN [ 36 ] := ' IXA' ;
        MN [ 37 ] := ' LAO' ;
        MN [ 38 ] := ' LCA' ;
        MN [ 39 ] := ' LDO' ;
        MN [ 40 ] := ' MOV' ;
        MN [ 41 ] := ' ALS' ;
        MN [ 42 ] := ' RET' ;
        MN [ 43 ] := ' SRO' ;
        MN [ 44 ] := ' XJP' ;
        MN [ 45 ] := ' CHK' ;
        MN [ 46 ] := ' CUP' ;
        MN [ 47 ] := ' EQU' ;
        MN [ 48 ] := ' GEQ' ;
        MN [ 49 ] := ' GRT' ;
        MN [ 50 ] := ' LDA' ;
        MN [ 51 ] := ' LDC' ;
        MN [ 52 ] := ' LEQ' ;
        MN [ 53 ] := ' LES' ;
        MN [ 54 ] := ' LOD' ;
        MN [ 55 ] := ' NEQ' ;
        MN [ 56 ] := ' STR' ;
        MN [ 57 ] := ' UJP' ;
        MN [ 58 ] := ' ORD' ;
        MN [ 59 ] := ' CHR' ;
        MN [ 60 ] := ' UJC' ;
        MN [ 61 ] := ' JPA' ;
        MN [ 62 ] := ' CXP' ;
        MN [ 63 ] := ' DUP' ;
        MN [ 64 ] := ' FTE' ;
        MN [ 65 ] := ' FTL' ;
        MN [ 66 ] := ' FDE' ;
        MN [ 67 ] := ' FDL' ;
      end (* INSTRMNEMONICS *) ;


   procedure CHARTYPES ;

      var I : INTEGER ;
          CH : CHAR ;

      begin (* CHARTYPES *)
        for I := ORDMINCHAR to ORDMAXCHAR do
          CHARTP [ CHR ( I ) ] := ILLEGAL ;
        for CH := 'A' to 'Z' do
          CHARTP [ CH ] := LETTER ;
        for CH := '0' to '9' do
          begin
            CHARTP [ CH ] := NUMBER ;
            ORDINT [ CH ] := ORD ( CH ) - ORD ( '0' ) ;
          end (* for *) ;
        CHARTP [ '+' ] := SPECIAL ;
        CHARTP [ '-' ] := SPECIAL ;
        CHARTP [ '*' ] := SPECIAL ;
        CHARTP [ '/' ] := SPECIAL ;
        CHARTP [ '(' ] := SPECIAL ;
        CHARTP [ ')' ] := SPECIAL ;
        CHARTP [ '$' ] := LETTER ;
        CHARTP [ '=' ] := SPECIAL ;
        CHARTP [ ' ' ] := SPECIAL ;
        CHARTP [ ',' ] := SPECIAL ;
        CHARTP [ '.' ] := SPECIAL ;
        CHARTP [ '''' ] := SPECIAL ;
        CHARTP [ '[' ] := SPECIAL ;
        CHARTP [ ']' ] := SPECIAL ;
        CHARTP [ ':' ] := SPECIAL ;
        CHARTP [ '@' ] := SPECIAL ;
        CHARTP [ ';' ] := SPECIAL ;
        CHARTP [ '<' ] := SPECIAL ;
        CHARTP [ '>' ] := SPECIAL ;
        CHARTP [ '^' ] := SPECIAL ;
        CHARTP [ '_' ] := LETTER ;
        CHARTP [ '{' ] := SPECIAL ;
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
        CDX [ 62 ] := 0 ;
        CDX [ 63 ] := 1 ;
        CDX [ 64 ] := - 2 ;
        CDX [ 65 ] := - 1 ;
        CDX [ 66 ] := - 2 ;
        CDX [ 67 ] := - 1 ;
        PDX [ 1 ] := - 1 ;
        PDX [ 2 ] := - 1 ;
        PDX [ 3 ] := - 2 ;
        PDX [ 4 ] := - 2 ;
        PDX [ 5 ] := - 2 ;
        PDX [ 6 ] := - 3 ;
        PDX [ 7 ] := - 3 ;
        PDX [ 8 ] := - 3 ;
        PDX [ 9 ] := - 3 ;
        PDX [ 10 ] := - 4 ;
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
        PDX [ 21 ] := - 1 ;
        PDX [ 22 ] := - 1 ;
        PDX [ 23 ] := - 1 ;
        PDX [ 24 ] := - 1 ;
        PDX [ 25 ] := - 1 ;
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
  REWRITE ( PRR ) ;
  FNAME := 'stdin' ;
  FNAMELEN := 5 ;
  INITSCALARS ;
  INITSETS ;
  INITTABLES ;
  LEVEL := 0 ;
  TOP := 0 ;
  LASTSTNEST := 0 ;
  STNEST := 0 ;
  MIN_LEVEL := 0 ;
  with DISPLAY [ 0 ] do
    begin
      FNAME := NIL ;
      FLABEL := NIL ;
      OCCUR := BLCK
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
      OCCUR := BLCK
    end (* with *) ;
  INSYMBOL ;
  PROGRAMME ( BLOCKBEGSYS + STATBEGSYS - [ CASESY ] ) ;
  ENDOFLINE ;
  if TOTALERRS > 0 then
    begin
      WRITELN ( '****  ' , TOTALERRS : 1 , ' Errors found' ) ;
      EXIT ( - 1 ) ;
    end (* then *) ;
end (* HAUPTPROGRAMM *) .
