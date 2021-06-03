program PCODE_TRANSLATOR ( PCODE , PCODE1 , PCODE2 , PCODE3 , OUTPUT ,
                           OBJCODE , LIST002 , TRACEF ) ;

(********************************************************************)
(*$D-,N+                                                            *)
(********************************************************************)



const IDLNGTH = 20 ;
      MXADR = 65535 ;


type ADRRNG = 0 .. MXADR ;
     BETA = array [ 1 .. 3 ] of CHAR ;
     DATATYPE = ( BOOL , CHRC , ADR , HINT , INT , PSET , REEL , PROC ,
                CARR , VARC , INX , NON ) ;
     PLABEL = record
                NAM : ALFA ;
                LEN : 0 .. IDLNGTH ;
                CADDR : ADRRNG ;
              end ;
     PPI = -> PROCEDURE_INFO ;
     PROCEDURE_INFO = record
                        CURPNAME : CHAR ( 20 ) ;
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
                        NEXT : PPI
                      end ;


var PCODEF : TEXT ;
    TRACEF : TEXT ;
    ERRORCNT : INTEGER ;
    LASTLN : INTEGER ;
    PIAKT : PPI ;
    CH : CHAR ;
    CH1 : CHAR ;
    TYPCDE : array [ 'A' .. 'Z' ] of DATATYPE ;
    MODUS : INTEGER ;
    PIANKER : PPI ;
    P : INTEGER ;



procedure ERROR_SYMB ( ERRCDE : INTEGER ; ERR_SYMBOL : BETA ) ;

   begin (* ERROR_SYMB *)
     ERRORCNT := ERRORCNT + 1 ;
     WRITELN ( OUTPUT , '   ++++ Error ' , ERRCDE : 5 , ' (near line '
               , LASTLN : 6 , ' of procedure ' , RTRIM ( PIAKT -> .
               CURPNAME ) , ')' ) ;
     WRITELN ( TRACEF , '   ++++ Error ' , ERRCDE : 5 , ' (near line '
               , LASTLN : 6 , ' of procedure ' , RTRIM ( PIAKT -> .
               CURPNAME ) , ')' ) ;
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



procedure SKIPBLANKS ;

   begin (* SKIPBLANKS *)
     GET ( PCODEF ) ;
     if EOL ( PCODEF ) then
       ERROR ( 618 ) ;
   end (* SKIPBLANKS *) ;



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



procedure READ_ENT ;

   var X : INTEGER ;
       OPTV : CHAR ( 8 ) ;

   begin (* READ_ENT *)

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
         PIAKT -> . OPNDTYPE := TYPCDE [ PCODEF -> ] ;
         READ ( PCODEF , CH1 , CH , P , CH ) ;
         READLBL ( PIAKT -> . SEGSZE ) ;
         if PCODEF -> = ' ' then
           SKIPBLANKS ;
         READ ( PCODEF , PIAKT -> . CURPNAME , CH ) ;

     //****************************************
     // change input format here - 10.05.2021
     //****************************************

         READ ( PCODEF , PIAKT -> . SAVERGS , CH ) ;
         if CH = ',' then
           begin
             READ ( PCODEF , PIAKT -> . ASM , CH ) ;
             READ ( PCODEF , PIAKT -> . GET_STAT , CH ) ;
             READ ( PCODEF , PIAKT -> . ASMVERB , CH ) ;
             PIAKT -> INIT_AUTO := FALSE ;
           end (* then *)
         else
           begin
             OPTV := ' ' ;
             OPTV [ 1 ] := CH ;
             X := 1 ;
             repeat
               X := X + 1 READ ( PCODEF , OPTV [ X ] )
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
         return ;
       end (* then *)
     else
       begin
         SKIPBLANKS ;
         READ ( PCODEF , CH1 , CH , P , CH ) ;
       end (* else *)
   end (* READ_ENT *) ;



begin (* HAUPTPROGRAMM *)

end (* HAUPTPROGRAMM *) .
