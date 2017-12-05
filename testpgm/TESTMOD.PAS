module TESTMOD ( QRD , OUTPUT ) ;

(********************************************************************)
(*$D-                                                               *)
(********************************************************************)
(*                                                                  *)
(*             SNAPSHOT AND POST MORTEM DUMP UTILITY                *)
(*                                                                  *)
(*  THIS PROGRAM PRINTS OUT THE CURRENT STACK ENVIRONMENT FOR A     *)
(*  PASCAL PROGRAM RUNNING ON THE S.L.A.C. TRIPLEX SYSTEM (IBM 370) *)
(*  SNAPSHOT TRACES BACK THROUGH THE STACK FRAMES OF THE PROGRAM    *)
(*  AND FOR EACH STACK FRAME PRINTS 1) THE NAME OF THE PROCEDURE    *)
(*  WHICH 'OWNS' THAT FRAME, 2) THE NAME OF THE PROCEDURE WHICH     *)
(*  CALLED THE OWNING PROCEDURE AND THE SOURCE LINE NUMBER FROM     *)
(*  WHICH THE CALL WAS MADE, 3) THE VALUES OF THE PROCEDURE'S       *)
(*  PARAMETERS AND LOCAL VARIABLES.                                 *)
(*                                                                  *)
(*  THIS PROGRAM IS CLOSELY AWARE OF THE RUN-TIME ENVIRONMENT OF    *)
(*  'PASCAL' PROGRAMS AND USES 'CASE VARIANT' RECORDS TO INTERPRET  *)
(*  THE CONTENTS OF MEMORY LOCATIONS DIFFERENTLY AS NEEDED. FOR     *)
(*  THIS REASON, IT SHOULD NOT BE COMPILED WITH THE 'D+' OPTION.    *)
(*                                                                  *)
(*  AUTHOR:  EARL WALDIN                                            *)
(*                                                                  *)
(*           COMPUTATION RESEARCH GROUP                             *)
(*           STANFORD LINEAR ACCELERATOR CENTER                     *)
(*           STANFORD, CA. 94305                                    *)
(*                                                                  *)
(*                                                                  *)
(*                                         UPDATED: 12-05-78  (SZH) *)
(*                                         UPDATED: 08-09-79  (SZH) *)
(*                                         UPDATED: 07-26-80  (RNH) *)
(*                                         UPDATED: 04-11-82  (RNH) *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  09.2016 - bernd.oppolzer@yahoo.com                              *)
(*                                                                  *)
(*  tested with the new Stanford compiler version on VM Rel. 6      *)
(*  SNAPSHOT is linked from PASMONN using WXTRN                     *)
(*  no problem so far                                               *)
(*  the file QRD is written from the compiler as file QRR           *)
(*  (symbol table for variables and procedures)                     *)
(*  The identifiers now can have up to 20 chars in length.          *)
(*                                                                  *)
(*  PNAMLN changed to 20 to show longer variable and procedure      *)
(*  names correctly                                                 *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  10.2016 - bernd.oppolzer@yahoo.com                              *)
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
(*  Other changes:                                                  *)
(*                                                                  *)
(*  - removed $ from internal proc names                            *)
(*                                                                  *)
(*  - to get access to the debug information which is at the        *)
(*    end of the Code CSECT, the length of the code is needed.      *)
(*    This was stored at EPA + 10 and overwrote part of the         *)
(*    CSECT info. I moved it behind the CSECT info, at the          *)
(*    position 2 bytes before the STATIC CSECT pointer, that        *)
(*    is: displacement of the initial branch instruction            *)
(*    minus 6. PASSNAP was changed at several places according      *)
(*    to this logic (see PTRADD-calls).                             *)
(*                                                                  *)
(*  - the calls to TRACER will work no more, because TRACER         *)
(*    still expects the code size at the old place (see             *)
(*    some places in PASMONN.ASS). This will be reworked later.     *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Dec 2016 - Extensions to the Compiler by Bernd Oppolzer         *)
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
(*  see procedure PRINT_VARIABLE and the calls of the               *)
(*  DUMP procedure in the NODEBUG case ...                          *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Apr 2017 - MVS Edition of PASSNAP - by Bernd Oppolzer           *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  To open the DBGINFO members with names known at runtime,        *)
(*  a new function ASSIGNMEM was created - and several              *)
(*  extensions have been made to the Pascal runtime PASMONN         *)
(*                                                                  *)
(*  After ASSIGNMEM and RESET, the File Status (offset 32           *)
(*  of the Pascal FCB) may be tested, and if it is '0',             *)
(*  the RESET was not successful, probably because the member       *)
(*  with the given name does not exist                              *)
(*                                                                  *)
(*  -------------------------------------------------------------   *)
(*                                                                  *)
(*  PASSNAP Edition for MVS = PASSNAP (easier for Linkage Editor)   *)
(*  PASSNAP Edition for CMS = PASSNAPC                              *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  May 2017 - Extensions to the Runtime by Bernd Oppolzer          *)
(*             (berndoppolzer@yahoo.com)                            *)
(*                                                                  *)
(*  The error handling in the RUNTIME has been corrected and        *)
(*  improved for the case when PASSNAP is NOT used; this            *)
(*  introduced some changes in the common interface.                *)
(*                                                                  *)
(*  Now in case of an error a common error structure is built       *)
(*  and used by PASMONN and PASSNAP; see DUMPPARM below.            *)
(*                                                                  *)
(*  It contains the PSW, the registers, informations for            *)
(*  certain non-system runtime error like SUBRANGE and              *)
(*  some interesting addresses from the Pascal runtime              *)
(*  (heap pointers etc.).                                           *)
(*                                                                  *)
(*  See the procedure PRINT_SYSD, which prints these infos.         *)
(*                                                                  *)
(********************************************************************)



type CHARPTR = -> CHAR ;



procedure CMSX ( CMD : CHARPTR ; var RETCODE : INTEGER ) ;

   EXTERNAL ;



function $PASLIB ( FUNCCODE : INTEGER ; X : CHARPTR ) : CHARPTR ;

   EXTERNAL ;



procedure $PASSNAP ( LEVEL : INTEGER ; DUMPPTR : VOIDPTR ) ;

(****************************************************************)
(*  VALUES FOR DMPKIND WHEN CALLED BY A USER PROCEDURE ARE:     *)
(*                                                              *)
(*  0 - FULL LISTING EXCEPT THAT FOR ARRAYS, ONLY THE FIRST     *)
(*       AND LAST FEW  ENTRIES ARE PRINTED. (DEFAULT ON ABEND)  *)
(*                                                              *)
(*  1 - TRACE OF PROCEDURE CALLS ONLY (NO VARIABLES).           *)
(*                                                              *)
(* 10 - FULL LISTING INCLUDING ALL ENTRIES IN ARRAYS.           *)
(****************************************************************)


   const HALFW = 65536 ;

         (********)
         (*2EXP16*)
         (********)

         SL24 = 16777216 ;

         (********)
         (*2EXP24*)
         (********)

         ADDRC = SL24 ;
         PROCSIZE = 8192 ;
         FILHDRSIZE = 8 ;

         (*********************)
         (*SIZE OF FILE HEADER*)
         (*********************)

         VARLN = 8 ;
         PNAMLN = 20 ;
         BYL_INT = 4 ;
         MAL_INT = 4 ;
         BYL_HINT = 2 ;
         BYL_PTR = 4 ;
         MAL_PTR = 4 ;
         BYL_REAL = 8 ;
         MAL_REAL = 8 ;
         BYL_BOOL = 1 ;
         MAL_BOOL = 1 ;
         BYL_CHAR = 1 ;
         MAL_CHAR = 1 ;
         BYL_SET = 4 ;
         MAL_SET = 4 ;
         MAXELEM = 255 ;

         (*********************************)
         (*LARGEST IMPLEMENTED SET ELEMENT*)
         (*********************************)

         ELSPW = 32 ;

         (*********************************)
         (*NO. OF SET ELEMENTS PER WORD   *)
         (*********************************)

         MXINX = 8 ;

         (*********************************)
         (* = (MAXELEM+1) DIV ELSPW       *)
         (*********************************)

         IBFLN = 256 ;
         MIDCOL = 45 ;

         (************************************)
         (* APPROX. MIDDLE COLUMN ON LISTING *)
         (************************************)

         DEFLEV = 20 ;

         (***************************************)
         (* DEFAULT MAX LEVEL OF STACK TO TRACE *)
         (***************************************)

         ARRLMT = 4 ;

         (***************************************)
         (* DEFAULT # OF ARRAY ELEMENTS PRINTED *)
         (***************************************)

         STRLMT = 140 ;

         (****************************************)
         (* LIMIT ON STRING (ARRAY OF CHAR) SIZE *)
         (****************************************)

         UNDFINT = - 2122219135 ;

         (******************)
         (* HEX '81818181' *)
         (******************)

         UNDFHINT = - 32383 ;

         (******************)
         (* HEX '8181'     *)
         (******************)

         UNDFBYTE = 129 ;

         (******************)
         (* HEX '81'       *)
         (******************)

         MAXPTRS = 500 ;

         (*********************************)
         (* NO. OF ITEMS ON HEAP TO PRINT *)
         (*********************************)

         FULL = 0 ;
         STKTRACE = 1 ;

   type CHAR8 = array [ 1 .. 8 ] of CHAR ;
        CHAR80 = array [ 1 .. 80 ] of CHAR ;
        INT2PTR = record
                    case INTEGER of
                      0 :
                        ( P : CHARPTR ) ;
                      1 :
                        ( I : INTEGER ) ;
                  end ;
        ABNDP = -> DUMPPARM ;
        DUMPPARM = record
                     CHKERRC : INTEGER ;
                     CHKPSW1 : VOIDPTR ;
                     CHKPSW2 : VOIDPTR ;
                     CHKREGS : array [ 0 .. 15 ] of VOIDPTR ;
                     CHKINTRP : VOIDPTR ;
                     CHKITYPE : CHAR ;
                     CHKRANGE : CHAR ;
                     CHKFILE : CHAR ;
                     CHKDUMMY : CHAR ;
                     CHKLOWR : INTEGER ;
                     CHKUPPR : INTEGER ;
                     CHKCVAL : INTEGER ;
                     CHKMSGL : INTEGER ;
                     CHKMSG : -> MSGTYPE ;
                     CHKPASE : VOIDPTR ;
                     CHKHEAPP : VOIDPTR ;
                     CHKHEAPT : VOIDPTR ;
                   end ;
        MSGTYPE = array [ 1 .. 100 ] of CHAR ;
        HINTEGER = - 32768 .. 32767 ;
        DUMMYRNG = 0 .. 10000 ;
        PROCNAME = array [ 1 .. PNAMLN ] of CHAR ;
        PTR_TAG = record
                    case INTEGER of
                      1 :
                        ( I : INTEGER ) ;
                      2 :
                        ( C : CHAR ;

        (***********************)
        (* HOLDS TYPE CODE TAG *)
        (***********************)

                          P : array [ 1 .. 3 ] of CHAR )
                  end ;
        PROC_PTR = -> PROC_HEAD ;
        PROCTPTR = -> PROC_TAIL ;
        FRM_PTR = -> STK_FRM ;
        STK_FRM = record
                    case INTEGER of

        (*************************)
        (* STACK FRAME MARK AREA *)
        (*************************)

                      1 :
                        ( SPC1 : INTEGER ;
                          BAK_LNK : FRM_PTR ;
                          FOR_LNK : FRM_PTR ;
                          RET : INTEGER ;
                          EPA : PROC_PTR ;
                          GPR0_12 : array [ 0 .. 12 ] of INTEGER ;
                          SPC2 , SPC3 : INTEGER ;
                          FPRS : array [ 1 .. 4 ] of REAL ) ;

        (*********************************)
        (* STACK FRAME VARIABLE OVERLAYS *)
        (*********************************)

                      2 :
                        ( case INTEGER of
                            1 :
                              ( INT_VA : array [ DUMMYRNG ] of INTEGER
                                ) ;
                            2 :
                              ( BOOL_VA : array [ DUMMYRNG ] of BOOLEAN
                                ) ;
                            3 :
                              ( CHR_VA : array [ DUMMYRNG ] of CHAR ) ;
                            4 :
                              ( REAL_VA : array [ DUMMYRNG ] of REAL )
                                ;
                            5 :
                              ( HINT_VA : array [ DUMMYRNG ] of
                                          HINTEGER ) ) ;

        (*********************)
        (* GLOBAL STACK AREA *)
        (*********************)

                      3 :
                        ( SPCB : array [ 1 .. 18 ] of INTEGER ;
                          CURNTNP : INTEGER ;
                          ENDNP0 : INTEGER )
                  end ;
        PROC_HEAD = record
                      SPC1 : INTEGER ;
                      SPNAME : array [ 0 .. 5 ] of CHAR ;
                    end ;
        PROC_TAIL = record
                      PROCID : HINTEGER ;
                      SLINNO : HINTEGER ;
                      SOURCENAME : CHAR8 ;
                      LINARY : array [ 1 .. 100000 ] of CHAR ;
                    end ;
        VARTYPE = ( INT , SCL , RL , BOL , CHA , PTR , PSET , REC , ARY
                  , FIL , PAD , UNK ) ;

   var DMARRAY : array [ 0 .. 1 ] of FRM_PTR ;

       (******************************************)
       (* caution ... tricky; the stack frame    *)
       (* pointers are fetched by array access   *)
       (* using certain negative indexes;        *)
       (* works only, because dmarray is the     *)
       (* very first variable (offset zero)      *)
       (******************************************)
       (* NIH - Bernd Oppolzer - 2017            *)
       (******************************************)

       DMPKIND : INTEGER ;
       VARTARY : packed array [ 'A' .. 'Z' ] of VARTYPE ;
       DEPTH , MAXDEPTH , I , J , UID , PLEN : INTEGER ;
       CH : CHAR ;
       DONE : BOOLEAN ;
       TOPSTK , BOTSTK , TMPSTK , STATICP : FRM_PTR ;
       CPROCN : PROCNAME ;
       IBF : array [ 1 .. IBFLN ] of CHAR ;
       PAR2 : INTEGER ;
       ARRYLSTF : BOOLEAN ;
       NUMPTRS , LASTPTR : 0 .. MAXPTRS ;
       PROCSTOCOME : set of 0 .. 255 ;
       HEXCHARS : array [ 0 .. 15 ] of CHAR ;
       PTRS : array [ 1 .. MAXPTRS ] of PTR_TAG ;
       ISDEBUG : BOOLEAN ;
       SOURCENAME : CHAR8 ;
       QRD_IS_OPEN : BOOLEAN ;
       DP : -> DUMPPARM ;
       X : INTEGER ;
       ERRMS : MSGTYPE ;


   procedure DUMPSTOR ( PVON : VOIDPTR ; PBIS : VOIDPTR ) ;

   (*********************************************************)
   (*  Speicherbereich von PVON bis PBIS hexadezimal        *)
   (*  ausgeben                                             *)
   (*********************************************************)


      var P1 : VOIDPTR ;
          P2 : VOIDPTR ;
          MOD1 : INTEGER ;
          MOD2 : INTEGER ;

      const DUMPSHOWCHARS : set of CHAR =
            [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' , 'A' .. 'I' , 'J'
              .. 'R' , 'S' .. 'Z' , '0' .. '9' , ' ' , ',' , '.' , '-'
              , ';' , ':' , '_' , '!' , '"' , 'õ' , '$' , '%' , '&' ,
              '/' , '(' , ')' , '=' , '?' , '+' , '*' , '#' , '*' ] ;


      procedure DUMPCHAR ( CH : CHAR ) ;

         begin (* DUMPCHAR *)
           if CH in DUMPSHOWCHARS then
             WRITE ( CH )
           else
             WRITE ( '.' )
         end (* DUMPCHAR *) ;


      procedure DUMPZEILE ( ADR : VOIDPTR ; P1 : VOIDPTR ; P2 : VOIDPTR
                          ) ;

         var CH : -> CHAR ;
             I : INTEGER ;
             J : INTEGER ;

         const HEXTAB : array [ 0 .. 15 ] of CHAR = '0123456789abcdef'
               ;

         begin (* DUMPZEILE *)
           WRITE ( ADR , ': ' ) ;
           CH := ADR ;
           for J := 1 to 4 do
             begin
               for I := 1 to 4 do
                 begin
                   if ( PTRDIFF ( CH , P1 ) < 0 ) or ( PTRDIFF ( CH ,
                   P2 ) > 0 ) then
                     WRITE ( '..' )
                   else
                     WRITE ( HEXTAB [ ORD ( CH -> ) DIV 16 ] , HEXTAB [
                             ORD ( CH -> ) MOD 16 ] ) ;
                   CH := PTRADD ( CH , 1 ) ;
                 end (* for *) ;
               WRITE ( ' ' ) ;
             end (* for *) ;
           CH := ADR ;
           WRITE ( ' *' ) ;
           for I := 1 to 16 do
             begin
               if ( PTRDIFF ( CH , P1 ) < 0 ) or ( PTRDIFF ( CH , P2 )
               > 0 ) then
                 WRITE ( ' ' )
               else
                 DUMPCHAR ( CH -> ) ;
               CH := PTRADD ( CH , 1 )
             end (* for *) ;
           WRITELN ( '*' ) ;
         end (* DUMPZEILE *) ;


      begin (* DUMPSTOR *)
        P1 := PTRADD ( PVON , - 16 ) ;
        MOD1 := PTR2INT ( P1 ) MOD 16 ;
        P1 := PTRADD ( P1 , 16 - MOD1 ) ;
        P2 := PTRADD ( PBIS , 16 ) ;
        MOD2 := PTR2INT ( P2 ) MOD 16 ;
        P2 := PTRADD ( P2 , - MOD2 ) ;
        while PTRDIFF ( P1 , P2 ) < 0 do
          begin
            DUMPZEILE ( P1 , PVON , PBIS ) ;
            P1 := PTRADD ( P1 , 16 ) ;
          end (* while *) ;
      end (* DUMPSTOR *) ;


   function IDLENGTH ( NAME : PROCNAME ) : INTEGER ;

      var I : INTEGER ;

      begin (* IDLENGTH *)
        I := PNAMLN ;
        while NAME [ I ] = ' ' do
          I := I - 1 ;
        IDLENGTH := I ;
      end (* IDLENGTH *) ;


   procedure PRINTHEX ( X : INTEGER ; L : INTEGER ) ;

      var IP : INT2PTR ;

      begin (* PRINTHEX *)
        IP . I := X ;
        WRITE ( IP . P : L ) ;
      end (* PRINTHEX *) ;


   procedure GET_PROCNAME ( P : PROC_PTR ; var PID , LEN : INTEGER ;
                          var NAME : PROCNAME ; var STATICP : FRM_PTR ;
                          var ISDEBUG : BOOLEAN ; var SOURCENAME :
                          CHAR8 ) ;

      type INTPTR = -> INTEGER ;

      var TP : record
                 case INTEGER of
                   1 :
                     ( I : INTEGER ) ;
                   2 :
                     ( P : PROCTPTR )
               end ;
          I : INTEGER ;
          OFFS : INTEGER ;
          PLEN : -> HINTEGER ;
          PSTATIC : -> FRM_PTR ;
          PDEBUG : -> HINTEGER ;
          LEN_NAME : INTEGER ;
          CP : -> CHAR ;
          COMP_SIG : array [ 1 .. 6 ] of CHAR ;
          LEN_KENNUNG : INTEGER ;

      begin (* GET_PROCNAME *)
        OFFS := P -> . SPC1 MOD 256 ;
        CP := PTRADD ( P , 4 ) ;
        LEN_KENNUNG := ORD ( CP -> ) ;
        repeat
          LEN_KENNUNG := LEN_KENNUNG + 1
        until LEN_KENNUNG MOD 4 = 2 ;
        CP := PTRADD ( CP , LEN_KENNUNG ) ;
        MEMCPY ( ADDR ( COMP_SIG ) , CP , 6 ) ;
        if COMP_SIG = 'STPASC' then
          begin
            PLEN := PTRADD ( P , OFFS ) ;
            PLEN := PTRADD ( PLEN , - 6 ) ;
            PSTATIC := PTRADD ( PLEN , 2 ) ;
            STATICP := PSTATIC -> ;
            PDEBUG := PTRADD ( PLEN , - 2 ) ;
            ISDEBUG := ( PDEBUG -> <> 0 ) ;
            LEN := PLEN -> ;
            PID := 0 ;
            NAME := ' ' ;
            SOURCENAME := ' ' ;
            LEN_NAME := ORD ( P -> . SPNAME [ 0 ] ) ;
            if LEN_NAME >= 29 then
              for I := 1 to PNAMLN do
                NAME [ I ] := P -> . SPNAME [ I + 9 ]
            else
              for I := 1 to 8 do
                NAME [ I ] := P -> . SPNAME [ I ] ;
            if ISDEBUG then
              begin
                TP . I := ORD ( P ) + LEN ;
                PID := TP . P -> . PROCID ;
                SOURCENAME := TP . P -> . SOURCENAME ;
              end (* then *) ;
          end (* then *)
        else
          begin
            NAME := '*UNKNOWN' ;
            STATICP := PTRADD ( NIL , 1 ) ;
            LEN := 0 ;
            PID := 0 ;
            ISDEBUG := FALSE ;
            SOURCENAME := ' '
          end (* else *)
      end (* GET_PROCNAME *) ;


   procedure PRINT_VARIABLE ( STKP : FRM_PTR ; STATICP : FRM_PTR ;
                            PNAME : PROCNAME ; PID : INTEGER ; ISDEBUG
                            : BOOLEAN ; SOURCENAME : CHAR8 ) ;

      label 999 ;

      type CMPCODE = ( PROCESS , SUCC , TYPERR , IDERR , SYNERR ,
                     NUMERR , EOFERR , BUFERR , ADDERR ) ;
           CNTRL = ( NAMED , UNNAMED , INDEXNAME , HEAPNAME ) ;

      var MAXI , I , VADDR , ALNFCT : INTEGER ;
          TPROCN : PROCNAME ;
          ERR : CMPCODE ;
          TN , NXT , NUM , J , LINELEN : INTEGER ;
          INDRCT : BOOLEAN ;
          SPKLASSE : CHAR ;
          H : array [ 1 .. 6 ] of CHAR ;
          SB : record
                 case INTEGER of
                   1 :
                     ( I : INTEGER ) ;
                   2 :
                     ( P : FRM_PTR )
               end ;
          CMSCMD : CHAR80 ;
          RC : INTEGER ;
          STATERC : INTEGER ;
          PSTATNAME : -> CHAR8 ;
          PSTATLEN : -> HINTEGER ;
          SSIZE : -> HINTEGER ;
          OFFS : HINTEGER ;
          AKT_PROC : PROC_PTR ;
          FCB : VOIDPTR ;
          CPT : CHARPTR ;


      procedure INS_SOURCENAME ( var CMD : CHAR80 ; SOURCENAME : CHAR8
                               ; STARTPOS : INTEGER ) ;

         begin (* INS_SOURCENAME *)
           for I := 1 to 8 do
             begin
               CMD [ STARTPOS ] := SOURCENAME [ I ] ;
               STARTPOS := STARTPOS + 1 ;
             end (* for *)
         end (* INS_SOURCENAME *) ;


      procedure ALIGN ( var OFFSET : INTEGER ; ALN : INTEGER ) ;

      (*******************************************************)
      (* ALIGNS POSITIVE 'OFFSET' ON AN 'ALN' BYTE BOUNDARY  *)
      (*******************************************************)


         begin (* ALIGN *)
           OFFSET := ( ( OFFSET + ALN - 1 ) DIV ALN ) * ALN ;
         end (* ALIGN *) ;


      procedure ERRMSG ( ECODE : CMPCODE ; INDX : INTEGER ) ;

         var J , K : INTEGER ;

         begin (* ERRMSG *)
           WRITELN ;
           WRITE ( '**** ERROR IN SYMBOL TABLE:    ' ) ;
           case ECODE of
             TYPERR :
               WRITELN ( 'ILLEGAL TYPE.' ) ;
             IDERR : WRITELN ( 'ILLEGAL IDENTIFIER.' ) ;
             SYNERR :
               WRITELN ( 'SYNTAX ERROR.' ) ;
             NUMERR :
               WRITELN ( 'IMPROPER NUMBER.' ) ;
             EOFERR :
               WRITELN ( 'PREMATURE END OF FILE.' ) ;
             BUFERR :
               WRITELN ( 'INTERNAL BUFFER EXCEEDED.' ) ;
             ADDERR :
               WRITELN ( 'ADDRESS EXPECTED.' ) ;
           end (* case *) ;
           J := 1 ;
           while J <= INDX do
             begin
               WRITELN ;
               WRITE ( ' ' ) ;
               if ( INDX - J + 1 ) < 80 then
                 K := INDX - J + 1
               else
                 K := 80 ;
               while K > 0 do
                 begin
                   WRITE ( IBF [ J ] ) ;
                   J := J + 1 ;
                   K := K - 1 ;
                 end (* while *) ;
             end (* while *) ;
           WRITELN ;
         end (* ERRMSG *) ;


      procedure PRINT ( INDNT : INTEGER ; var INDX : INTEGER ; SPKLASSE
                      : CHAR ; var OFFSET : INTEGER ; BASE : FRM_PTR ;
                      ARRINX : INTEGER ; IDCNTRL : CNTRL ) ;

         const DASHES = '    --------------------' ;

         var M , L , K , J , I , SAVALN : INTEGER ;
             UBND , LMT , LMT2 , LMT3 , LMT4 : INTEGER ;
             I_R_S : record
                       case INTEGER of
                         1 :
                           ( R : REAL ) ;
                         2 :
                           ( I1 : INTEGER ;
                             I2 : INTEGER ) ;
                     end ;
             TBOL : BOOLEAN ;
             TCH : CHAR ;
             H : array [ 1 .. 6 ] of CHAR ;
             IDNAME : array [ 1 .. PNAMLN ] of CHAR ;
             VTY : VARTYPE ;
             S_I : record
                     case INTEGER of
                       1 :
                         ( S : set of 0 .. MAXELEM ) ;
                       2 :
                         ( I : array [ 1 .. MXINX ] of INTEGER ) ;
                   end ;
             UNDF : BOOLEAN ;
             IP : INT2PTR ;


         procedure PRINT_UDEF ;

            const TN : array [ INT .. PSET ] of record L : 1 .. 8 ;
                  N : array [ 1 .. 8 ] of CHAR end =
                  ( ( 7 , 'INTEGER ' ) , ( 6 , 'SCALAR  ' ) , ( 4 ,
                    'REAL    ' ) , ( 7 , 'BOOLEAN ' ) , ( 4 ,
                    'CHAR    ' ) , ( 7 , 'POINTER ' ) , ( 3 ,
                    'SET     ' ) ) ;

            var LEN : 1 .. 8 ;

            begin (* PRINT_UDEF *)
              LEN := TN [ VTY ] . L ;
              WRITE ( ' = UNDEF. ' , TN [ VTY ] . N : LEN , ' ' ) ;
              LINELEN := LINELEN + LEN + 11 ;
            end (* PRINT_UDEF *) ;


         function CONVERT_NUM ( var IX : INTEGER ) : INTEGER ;

            var NUM , J : INTEGER ;
                SIGN : BOOLEAN ;

            begin (* CONVERT_NUM *)
              J := IX ;
              NUM := 0 ;
              SIGN := TRUE ;
              while IBF [ J ] = ' ' do
                J := J + 1 ;
              if IBF [ J ] = '-' then
                begin
                  SIGN := FALSE ;
                  J := J + 1 ;
                end (* then *) ;
              while IBF [ J ] in [ '0' .. '9' ] do
                begin
                  NUM := 10 * NUM + ORD ( IBF [ J ] ) - ORD ( '0' ) ;
                  J := J + 1 ;
                end (* while *) ;
              IX := J ;
              if SIGN then
                CONVERT_NUM := NUM
              else
                CONVERT_NUM := - NUM ;
            end (* CONVERT_NUM *) ;


         begin (* PRINT *)
           if IDCNTRL = NAMED then
             begin
               I := 0 ;
               IDNAME := '            ' ;
               while IBF [ INDX ] <> '=' do
                 begin
                   I := I + 1 ;
                   IDNAME [ I ] := IBF [ INDX ] ;
                   INDX := INDX + 1
                 end (* while *) ;
               INDX := INDX + 1 ;
             end (* then *) ;
           VTY := VARTARY [ IBF [ INDX ] ] ;
           if VTY in [ ARY , REC , PSET ] then
             if IDCNTRL <> HEAPNAME then
               LINELEN := MIDCOL ;

           (*******************************************************)
           (*    versuchsweise jede Zeile writeln ...             *)
           (*                                                     *)
           (*       if LINELEN >= MIDCOL then                     *)
           (*         begin                                       *)
           (*           WRITELN ;                                 *)
           (*           LINELEN := INDNT * 2 + 4 ;                *)
           (*           WRITE ( DASHES : LINELEN ) ;              *)
           (*         end                                         *)
           (*       else                                          *)
           (*         begin                                       *)
           (*           WRITE ( ' ' : MIDCOL + 4                  *)
           (*                   - LINELEN + INDNT * 2 ) ;         *)
           (*           LINELEN := MIDCOL + 2                     *)
           (*         end                                         *)
           (*******************************************************)

           WRITELN ;
           LINELEN := INDNT * 2 + 3 ;
           WRITE ( DASHES : LINELEN ) ;

           (*******************************************************)
           (*    Ende Versuch                                     *)
           (*-----------------------------------------------------*)
           (*    hier: Speicherklasse hinzugefuegt                *)
           (*******************************************************)

           LINELEN := LINELEN + PNAMLN + 19 ;
           case IDCNTRL of
             NAMED : begin
                       WRITE ( IDNAME : PNAMLN + 2 ) ;
                       WRITE ( '(' , SPKLASSE , '/' ) ;
                       PRINTHEX ( OFFSET , 4 ) ;
                       WRITE ( '/' , PTRADD ( BASE , OFFSET ) , ')' ) ;
                     end (* tag/ca *) ;
             UNNAMED :
               WRITE ( ' ' : PNAMLN + 19 ) ;
             INDEXNAME :
               WRITE ( ARRINX : PNAMLN + 18 , ':' ) ;
           end (* case *) ;

           (*******************************************************)
           (*    ausserdem adresse und offset                     *)
           (*******************************************************)

           INDX := INDX + 1 ;
           case VTY of
             INT , SCL :
               begin
                 J := ORD ( IBF [ INDX ] ) - ORD ( '0' ) ;
                 INDX := INDX + 1 ;
                 ALIGN ( OFFSET , J ) ;
                 ALIGN ( ALNFCT , J ) ;
                 if J = BYL_INT then
                   begin
                     I := BASE -> . INT_VA [ OFFSET DIV BYL_INT ] ;
                     UNDF := I = UNDFINT ;
                   end (* then *)
                 else
                   if J = BYL_HINT then
                     begin
                       I := BASE -> . HINT_VA [ OFFSET DIV BYL_HINT ] ;
                       UNDF := I = UNDFHINT ;
                     end (* then *)
                   else

           (****************)
           (* J = BYL_CHAR *)
           (****************)

                     begin
                       I := ORD ( BASE -> . CHR_VA [ OFFSET ] ) ;
                       UNDF := FALSE ;
                     end (* else *) ;
                 if UNDF then
                   PRINT_UDEF
                 else
                   if ( - 999999 <= I ) and ( I <= 9999999 ) then
                     begin
                       WRITE ( ' =' , I : 8 ) ;
                       LINELEN := LINELEN + 10
                     end (* then *)
                   else
                     begin
                       WRITE ( ' =' , I : 14 ) ;
                       LINELEN := LINELEN + 14 ;
                     end (* else *) ;
                 OFFSET := OFFSET + J ;
               end (* tag/ca *) ;
             RL : begin
                    ALIGN ( OFFSET , BYL_REAL ) ;
                    ALIGN ( ALNFCT , BYL_REAL ) ;
                    I_R_S . R := BASE -> . REAL_VA [ OFFSET DIV
                                 BYL_REAL ] ;
                    if ( I_R_S . I1 = UNDFINT ) and ( I_R_S . I2 =
                    UNDFINT ) then
                      PRINT_UDEF
                    else
                      begin
                        WRITE ( ' =' , I_R_S . R : 14 ) ;
                        LINELEN := LINELEN + 16
                      end (* else *) ;
                    OFFSET := OFFSET + BYL_REAL ;
                  end (* tag/ca *) ;
             CHA : begin
                     WRITE ( ' =     ''' ) ;

           (**********************************)
           (* LENGTH=1 --> NO ALIGNMENT HERE *)
           (**********************************)

                     TCH := BASE -> . CHR_VA [ OFFSET ] ;
                     if ( 64 <= ORD ( TCH ) ) and ( ORD ( TCH ) <= 250
                     ) then
                       WRITE ( TCH , '''' )
                     else
                       WRITE ( '#''' ) ;
                     LINELEN := LINELEN + 10 ;
                     OFFSET := OFFSET + BYL_CHAR ;
                   end (* tag/ca *) ;
             BOL : begin

           (**********************************)
           (* LENGTH=1 --> NO ALIGNMENT HERE *)
           (**********************************)

                     TBOL := BASE -> . BOOL_VA [ OFFSET ] ;
                     if ORD ( TBOL ) = UNDFBYTE then
                       PRINT_UDEF
                     else
                       begin
                         LINELEN := LINELEN + 10 ;
                         if TBOL then
                           WRITE ( ' =   TRUE ' )
                         else
                           WRITE ( ' =   FALSE' ) ;
                       end (* else *) ;
                     OFFSET := OFFSET + BYL_BOOL ;
                   end (* tag/ca *) ;
             PTR : begin
                     ALIGN ( OFFSET , BYL_PTR ) ;
                     ALIGN ( ALNFCT , BYL_PTR ) ;
                     I := CONVERT_NUM ( INDX ) ;

           (***************)
           (*TYPE CODE NO.*)
           (***************)

                     J := BASE -> . INT_VA [ OFFSET DIV BYL_INT ] ;
                     IP . I := J ;
                     repeat
                       if J = - 1 then
                         begin
                           WRITE ( ' =     NIL' ) ;
                           LINELEN := LINELEN + 10 ;
                           break ;
                         end (* then *) ;
                       if J = UNDFINT then
                         begin
                           PRINT_UDEF ;
                           break ;
                         end (* then *) ;
                       if ( BOTSTK -> . CURNTNP <= J ) and ( J <=
                       BOTSTK -> . ENDNP0 ) then
                         begin
                           WRITE ( ' = ' , IP . P ) ;
                           WRITE ( ' (HEAP/NEW)' ) ;
                           LINELEN := LINELEN + 22 ;
                           if NUMPTRS < MAXPTRS then
                             if I in [ 0 .. 255 ] then
                               begin
                                 PTRS [ NUMPTRS + 1 ] . I := J ;
                                 K := 1 ;
                                 while PTRS [ K ] . P <> PTRS [ NUMPTRS
                                 + 1 ] . P do
                                   K := K + 1 ;
                                 if K > NUMPTRS then
                                   NUMPTRS := K ;
                                 PTRS [ K ] . C := CHR ( I ) ;
                               end (* then *) ;
                           break ;
                         end (* then *) ;
                       if $PASLIB ( 6 , IP . P ) <> NIL then
                         begin
                           WRITE ( ' = ' , IP . P ) ;
                           WRITE ( ' (HEAP/ALLOC)' ) ;
                           LINELEN := LINELEN + 24 ;
                           if NUMPTRS < MAXPTRS then
                             if I in [ 0 .. 255 ] then
                               begin
                                 PTRS [ NUMPTRS + 1 ] . I := J ;
                                 K := 1 ;
                                 while PTRS [ K ] . P <> PTRS [ NUMPTRS
                                 + 1 ] . P do
                                   K := K + 1 ;
                                 if K > NUMPTRS then
                                   NUMPTRS := K ;
                                 PTRS [ K ] . C := CHR ( I ) ;
                               end (* then *) ;
                           break ;
                         end (* then *) ;

           (*******************)
           (* BAD/OLD POINTER *)
           (*******************)

                       WRITE ( ' = ' , IP . P , ' (INVALID POINTER)' )
                               ;
                       LINELEN := LINELEN + 29
                     until TRUE ;
                     OFFSET := OFFSET + BYL_INT ;
                   end (* tag/ca *) ;
             PSET : begin
                      ALIGN ( OFFSET , MAL_SET ) ;
                      ALIGN ( ALNFCT , MAL_SET ) ;
                      I := OFFSET DIV BYL_SET ;
                      M := CONVERT_NUM ( INDX ) ;
                      L := CONVERT_NUM ( INDX ) ;
                      L := ( L + ELSPW - 1 ) DIV ELSPW ;
                      S_I . S := [ ] ;
                      UNDF := TRUE ;
                      for J := 1 to L do
                        begin
                          S_I . I [ J ] := BASE -> . INT_VA [ I + J - 1
                                           ] ;
                          if S_I . I [ J ] <> UNDFINT then
                            UNDF := FALSE ;
                        end (* for *) ;
                      if UNDF then
                        PRINT_UDEF
                      else
                        begin
                          K := 0 ;
                          TCH := '(' ;
                          WRITE ( ' = ' ) ;
                          repeat
                            if K in S_I . S then
                              begin
                                WRITE ( TCH , K : 1 ) ;
                                J := K ;
                                while ( J < MAXELEM ) and ( ( J + 1 )
                                in S_I . S ) do
                                  J := J + 1 ;
                                if J > K then
                                  if J > ( K + 1 ) then
                                    WRITE ( '..' , J : 1 )
                                  else
                                    WRITE ( ',' , J : 1 ) ;
                                TCH := ',' ;
                                K := J + 2 ;
                              end (* then *)
                            else
                              K := K + 1 ;
                          until K > MAXELEM ;
                          if TCH = '(' then
                            begin
                              WRITE ( 'EMPTY' ) ;
                              LINELEN := LINELEN + 8
                            end (* then *)
                          else
                            begin
                              WRITE ( ')' ) ;
                              LINELEN := MIDCOL
                            end (* else *) ;
                        end (* else *) ;
                      OFFSET := OFFSET + L * BYL_SET ;
                    end (* tag/ca *) ;
             REC : begin
                     WRITE ( ' = RECORD' ) ;
                     J := ORD ( IBF [ INDX ] ) - ORD ( '0' ) ;
                     INDX := INDX + 2 ;
                     ALIGN ( OFFSET , J ) ;
                     ALIGN ( ALNFCT , J ) ;
                     INDNT := INDNT + 1 ;
                     LINELEN := MIDCOL ;
                     if IBF [ INDX - 1 ] = '(' then
                       while IBF [ INDX ] <> ')' do
                         begin
                           PRINT ( INDNT , INDX , 'R' , OFFSET , BASE ,
                                   0 , NAMED ) ;
                         end (* while *) ;
                     INDX := INDX + 1 ;
                     LINELEN := MIDCOL ;
                   end (* tag/ca *) ;
             ARY : begin
                     K := CONVERT_NUM ( INDX ) ;
                     M := CONVERT_NUM ( INDX ) ;
                     INDX := INDX + 1 ;
                     UBND := M - K ;
                     if ( VARTARY [ IBF [ INDX ] ] = CHA ) and ( UBND
                     <= STRLMT ) then
                       begin
                         WRITE ( ' = ''' ) ;
                         for I := 0 to UBND do
                           begin
                             TCH := BASE -> . CHR_VA [ OFFSET ] ;
                             OFFSET := OFFSET + 1 ;
                             if ( 64 <= ORD ( TCH ) ) and ( ORD ( TCH )
                             <= 250 ) then
                               WRITE ( TCH )
                             else
                               WRITE ( '#' ) ;
                           end (* for *) ;
                         WRITE ( '''' ) ;
                         LINELEN := LINELEN + UBND + 6 ;
                         INDX := INDX + 1 ;
                       end (* then *)
                     else
                       begin

           (*******************************************)
           (* FIND THE RANGE OF INDICES TO BE PRINTED *)
           (*******************************************)

                         LMT := ARRLMT ;
                         if UBND <= 3 * ARRLMT then
                           LMT := ARRLMT * 2 ;

           (*****************************************)
           (* SHORT ARRAYS, PRINT THE WHOLE THING,  *)
           (* VERY LONG ARRAYS, PRINT MORE ELEMENTS *)
           (*****************************************)

                         LMT4 := UBND - LMT ;
                         LMT2 := LMT4 DIV 2 ;
                         LMT3 := LMT2 + LMT + 1 ;
                         SAVALN := ALNFCT ;
                         ALNFCT := BYL_CHAR ;

           (********)
           (*=1    *)
           (********)

                         LINELEN := MIDCOL ;
                         for I := 0 to UBND do
                           if ( I < LMT ) or ( ( LMT2 < I ) and ( I <
                           LMT3 ) ) or ( I > LMT4 ) or ARRYLSTF then
                             begin
                               J := INDX ;
                               L := OFFSET ;
                               PRINT ( INDNT + 1 , J , 'X' , OFFSET ,
                                       BASE , K + I , INDEXNAME ) ;
                               ALIGN ( OFFSET , ALNFCT ) ;
                               L := OFFSET - L ;

           (***************************)
           (* LENGTH OF ARRAY ELEMENT *)
           (***************************)

                             end (* then *)
                           else
                             begin
                               if ( I = LMT ) or ( I = LMT3 ) then
                                 WRITELN ;
                               J := INDX ;
                               LINELEN := MIDCOL ;
                               OFFSET := OFFSET + L ;

           (****************************)
           (* SKIP OVER OTHER ELEMENTS *)
           (****************************)

                             end (* else *) ;
                         INDX := J - 1 ;
                         ALIGN ( ALNFCT , SAVALN ) ;

           (***************************)
           (* UPDATE ALIGNMENT FACTOR *)
           (***************************)

                         LINELEN := MIDCOL ;
                       end (* else *) ;
                   end (* tag/ca *) ;
             FIL : begin
                     INDX := INDX + 1 ;
                     WRITE ( ': FILE COMPONENT' ) ;
                     ALIGN ( OFFSET , BYL_INT ) ;
                     ALIGN ( ALNFCT , BYL_INT ) ;
                     OFFSET := OFFSET + FILHDRSIZE ;
                     LINELEN := LINELEN + 16 ;
                     PRINT ( INDNT + 1 , INDX , 'F' , OFFSET , BASE , 0
                             , UNNAMED ) ;
                   end (* tag/ca *) ;
             PAD : begin

           (**********************)
           (* SKIP PADDING BYTES *)
           (**********************)

                     K := CONVERT_NUM ( INDX ) ;
                     OFFSET := OFFSET + K ;
                   end (* tag/ca *) ;
           end (* case *) ;
           INDX := INDX + 1 ;
         end (* PRINT *) ;


      function IVSCAN ( var IX : INTEGER ; IDREQ : BOOLEAN ) : CMPCODE
                      ;

      /***************************************************/
      /* implemented using gotos                         */
      /* rework, if spare time                           */
      /***************************************************/


         label 10 , 20 , 30 , 40 ;

         var NEST : 0 .. 20 ;
             INDX : INTEGER ;
             NUMOK : BOOLEAN ;
             VTYPE : VARTYPE ;


         procedure SCAN_INTEGER ;

            begin (* SCAN_INTEGER *)
              INDX := INDX + 1 ;
              IBF [ INDX ] := ' ' ;
              if EOT ( QRD ) then
                begin
                  NUMOK := FALSE ;
                  return ;
                end (* then *) ;
              if QRD -> in [ '0' .. '9' ] then
                repeat
                  INDX := INDX + 1 ;
                  if INDX > IBFLN - 4 then
                    begin
                      NUMOK := FALSE ;
                      return ;
                    end (* then *) ;
                  IBF [ INDX ] := QRD -> ;
                  GET ( QRD ) ;
                until not ( QRD -> in [ '0' .. '9' ] )
              else
                NUMOK := FALSE ;
            end (* SCAN_INTEGER *) ;


         begin (* IVSCAN *)
           IVSCAN := EOFERR ;
           NEST := 0 ;
           NUMOK := TRUE ;
           INDX := IX ;
           if IDREQ then
             begin
               30 :
               if EOT ( QRD ) then
                 goto 10 ;
               if QRD -> in [ 'A' .. 'Z' , '$' , '_' ] then
                 repeat
                   INDX := INDX + 1 ;
                   if INDX > IBFLN - 4 then
                     begin
                       IVSCAN := BUFERR ;
                       goto 10
                     end (* then *) ;
                   IBF [ INDX ] := QRD -> ;
                   GET ( QRD ) ;
                   if EOF ( QRD ) then
                     goto 10 ;
                 until not ( QRD -> in [ 'A' .. 'Z' , '$' , '_' , '0'
                 .. '9' ] )
               else
                 if ( NEST > 0 ) and ( QRD -> = ')' ) then
                   begin
                     NEST := NEST - 1 ;
                     GET ( QRD ) ;
                     INDX := INDX + 1 ;
                     IBF [ INDX ] := ')' ;
                     goto 40 ;
                   end (* then *)
                 else
                   begin
                     IVSCAN := IDERR ;
                     goto 10
                   end (* else *) ;
               if EOT ( QRD ) then
                 goto 10 ;
               if QRD -> <> '=' then
                 begin
                   IVSCAN := SYNERR ;
                   goto 10
                 end (* then *) ;
               INDX := INDX + 1 ;
               IBF [ INDX ] := '=' ;
               GET ( QRD ) ;
             end (* then *) ;
           20 :
           if EOT ( QRD ) then
             goto 10 ;
           if QRD -> in [ 'A' .. 'Z' ] then
             VTYPE := VARTARY [ QRD -> ]
           else
             VTYPE := UNK ;
           INDX := INDX + 1 ;
           if INDX > IBFLN - 4 then
             begin
               IVSCAN := BUFERR ;
               goto 10
             end (* then *) ;
           IBF [ INDX ] := QRD -> ;
           GET ( QRD ) ;
           case VTYPE of
             UNK : begin
                     IVSCAN := TYPERR ;
                     goto 10
                   end (* tag/ca *) ;
             INT , SCL :
               if QRD -> in [ '1' , '2' , '4' ] then
                 begin
                   INDX := INDX + 1 ;
                   IBF [ INDX ] := QRD -> ;
                   GET ( QRD )
                 end (* then *)
               else
                 begin
                   INDX := INDX + 1 ;
                   IBF [ INDX ] := '4'
                 end (* else *) ;
             CHA , BOL :
               ;

           (**********************)
           (* NO ACTION REQUIRED *)
           (**********************)

             FIL : begin
                     INDX := INDX + 1 ;
                     IBF [ INDX ] := ' ' ;
                     goto 20
                   end (* tag/ca *) ;
             PSET : begin
                      SCAN_INTEGER ;
                      SCAN_INTEGER ;
                      if not NUMOK then
                        goto 10
                    end (* tag/ca *) ;
             ARY : begin
                     SCAN_INTEGER ;
                     SCAN_INTEGER ;
                     if not NUMOK then
                       goto 10 ;
                     INDX := INDX + 1 ;
                     IBF [ INDX ] := ' ' ;
                     goto 20
                   end (* tag/ca *) ;
             PTR , PAD :
               begin
                 SCAN_INTEGER ;
                 if not NUMOK then
                   goto 10
               end (* tag/ca *) ;
             REC : begin
                     INDX := INDX + 1 ;
                     if QRD -> in [ '1' , '2' , '4' , '8' ] then
                       begin
                         IBF [ INDX ] := QRD -> ;
                         GET ( QRD )
                       end (* then *)
                     else
                       IBF [ INDX ] := '1' ;
                     if EOT ( QRD ) then
                       goto 10 ;
                     if QRD -> = '(' then
                       begin
                         GET ( QRD ) ;
                         NEST := NEST + 1 ;
                         INDX := INDX + 1 ;
                         IBF [ INDX ] := '(' ;
                         IDREQ := TRUE ;
                         goto 30
                       end (* then *)
                     else
                       begin
                         IVSCAN := SYNERR ;
                         goto 10
                       end (* else *) ;
                   end (* tag/ca *) ;
           end (* case *) ;
           if EOT ( QRD ) then
             goto 10 ;
           40 :
           if QRD -> <> ';' then
             begin
               IVSCAN := SYNERR ;
               goto 10
             end (* then *) ;
           INDX := INDX + 1 ;
           IBF [ INDX ] := ';' ;
           GET ( QRD ) ;
           if NEST > 0 then
             goto 30 ;
           IVSCAN := SUCC ;
           10 :
           if not NUMOK then
             IVSCAN := NUMERR ;
           IX := INDX ;
         end (* IVSCAN *) ;


      begin (* PRINT_VARIABLE *)
        AKT_PROC := STKP -> . EPA ;
        OFFS := AKT_PROC -> . SPC1 MOD 256 ;
        SSIZE := PTRADD ( AKT_PROC , OFFS ) ;
        SSIZE := PTRADD ( SSIZE , - 10 ) ;
        WRITELN ;
        WRITELN ( '**** VARIABLES FOR ' , PNAME : IDLENGTH ( PNAME ) ,
                  ' ****' ) ;
        WRITELN ( '     Stack at address ' , STKP ) ;
        if PTR2INT ( STATICP ) = 0 then
          WRITELN ( '     No static variables' )
        else
          WRITELN ( '     Static variables at address ' , STATICP ) ;
        if not ISDEBUG then
          begin
            WRITELN ( '     Procedure ' , PNAME : IDLENGTH ( PNAME ) ,
                      ' compiled without DEBUG option' ) ;
            WRITELN ( '     Areas can only be dumped ' 'in hex ' ) ;
            WRITELN ;
            WRITELN ( '**** Stack Area' , ' (Length = ' , SSIZE -> : 1
                      , ') ****' ) ;
            WRITELN ;
            DUMPSTOR ( PTRCAST ( STKP ) , PTRADD ( STKP , SSIZE -> - 1
                       ) ) ;
            if PTR2INT ( STATICP ) <> 0 then
              begin
                PSTATNAME := PTRCAST ( STATICP ) ;
                PSTATLEN := PTRADD ( STATICP , 8 ) ;
                WRITELN ;
                WRITELN ( '**** Static Area ' , PSTATNAME -> : 8 ,
                          ' (Length = ' , PSTATLEN -> : 1 , ') ****' )
                          ;
                WRITELN ;
                DUMPSTOR ( PTRCAST ( STATICP ) , PTRADD ( STATICP ,
                           PSTATLEN -> - 1 ) ) ;
              end (* then *) ;
            return
          end (* then *) ;

        (*******************************************)
        (* the following part is different between *)
        (* the mvs and cms versions ... opening    *)
        (* the dbginfo files depending on the      *)
        (* procedure name ...                      *)
        (*******************************************)

        if not ( PID in PROCSTOCOME ) then
          begin
            if QRD_IS_OPEN then
              CLOSE ( QRD ) ;
            QRD_IS_OPEN := FALSE ;
            if FALSE then
              WRITELN ( 'print_variable: try reset(qrd), sourcename = '
                        , SOURCENAME ) ;
            CMSCMD := 'STATE XXXXXXXX DBGINFO * #' ;
            INS_SOURCENAME ( CMSCMD , SOURCENAME , 7 ) ;
            CMSX ( ADDR ( CMSCMD ) , RC ) ;
            if FALSE then
              begin
                WRITELN ( 'CMSX: ' , CMSCMD ) ;
                WRITELN ( 'print_variable: state command returns rc = '
                          , RC ) ;
              end (* then *) ;
            STATERC := RC ;
            CMSCMD := 'FILEDEF QRD CLEAR #' ;
            CMSX ( ADDR ( CMSCMD ) , RC ) ;
            if STATERC = 0 then
              begin
                CMSCMD := 'FILEDEF QRD DISK XXXXXXXX DBGINFO * '
                          '(RECFM F LRECL 80#' ;
                INS_SOURCENAME ( CMSCMD , SOURCENAME , 18 ) ;
                CMSX ( ADDR ( CMSCMD ) , RC ) ;
                if FALSE then
                  WRITELN ( 'print_variable: filedef returns rc = ' ,
                            RC ) ;
                RESET ( QRD ) ;
                PROCSTOCOME := [ 0 .. 255 ] ;
                QRD_IS_OPEN := TRUE ;
              end (* then *)
            else
              begin
                WRITELN ( '     Debug information file for '
                          'program/module ' , SOURCENAME , ' not found'
                          ) ;
                WRITELN ;
                return ;
              end (* else *) ;
          end (* then *) ;

        (*******************************************)
        (* end of opsys dependency                 *)
        (*******************************************)

        TPROCN := '            ' ;
        VADDR := - 1 ;
        while ( ( PNAME <> TPROCN ) or ( PID <> VADDR ) ) and ( not EOF
        ( QRD ) ) do
          begin
            while ( QRD -> <> '%' ) and ( not EOF ( QRD ) ) do
              READLN ( QRD ) ;
            GET ( QRD ) ;

        (*****************)
        (* SKIP OVER "%" *)
        (*****************)

            if not EOT ( QRD ) then
              begin
                I := 1 ;
                TPROCN := '            ' ;
                while ( QRD -> <> ' ' ) do
                  if I <= PNAMLN then
                    begin
                      READ ( QRD , TPROCN [ I ] ) ;
                      I := I + 1
                    end (* then *)
                  else
                    GET ( QRD ) ;
                READLN ( QRD , VADDR ) ;
                if VADDR in [ 0 .. 255 ] then
                  PROCSTOCOME := PROCSTOCOME - [ VADDR ] ;
              end (* then *) ;
          end (* while *) ;
        if ( PNAME = TPROCN ) and ( PID = VADDR ) then
          begin
            ERR := SUCC ;
            if EOT ( QRD ) then
              goto 999 ;
            LINELEN := MIDCOL ;
            while QRD -> in [ '@' , '0' .. '9' ] do
              begin
                if QRD -> = '@' then
                  begin
                    INDRCT := TRUE ;
                    GET ( QRD ) ;
                    if EOT ( QRD ) then
                      begin
                        ERR := EOFERR ;
                        goto 999
                      end (* then *) ;
                    if not ( QRD -> in [ '0' .. '9' ] ) then
                      begin
                        ERR := ADDERR ;
                        goto 999
                      end (* then *) ;
                  end (* then *)
                else
                  INDRCT := FALSE ;
                MAXI := 0 ;
                READ ( QRD , VADDR ) ;
                while QRD -> = ' ' do
                  GET ( QRD ) ;
                SPKLASSE := QRD -> ;
                GET ( QRD ) ;
                ERR := IVSCAN ( MAXI , TRUE ) ;
                if ERR <> SUCC then
                  goto 999 ;
                I := 1 ;
                ALNFCT := MAL_CHAR ;

        (*******************************)
        (* INITIALIZE ALIGNMENT FACTOR *)
        (*******************************)

                if INDRCT then
                  begin
                    SB . I := STKP -> . INT_VA [ VADDR DIV BYL_INT ] ;
                    VADDR := 0 ;
                    if ( ORD ( BOTSTK ) <= SB . I ) and ( SB . I <=
                    BOTSTK -> . ENDNP0 ) then
                      PRINT ( 0 , I , 'A' , VADDR , SB . P , 0 , NAMED
                              ) ;
                  end (* then *)
                else
                  if SPKLASSE = 'A' then
                    begin
                      SB . P := STKP ;
                      PRINT ( 0 , I , 'A' , VADDR , SB . P , 0 , NAMED
                              )
                    end (* then *)
                  else
                    if SPKLASSE = 'S' then
                      if PTR2INT ( STATICP ) > 0 then
                        begin
                          SB . P := STATICP ;
                          PRINT ( 0 , I , 'S' , VADDR , SB . P , 0 ,
                                  NAMED )
                        end (* then *) ;
                if EOT ( QRD ) then
                  goto 999 ;
              end (* while *) ;
            if ( QRD -> = '>' ) and ( NUMPTRS > 0 ) then
              begin

        (****************************)
        (* PRINT HEAP STORAGE ITEMS *)
        (****************************)

                NXT := BOTSTK -> . CURNTNP ;
                TN := 0 ;
                while QRD -> = '>' do

        (***********************************)
        (* FIRST READ THE HEAP TYPE DEFN.S *)
        (***********************************)

                  begin
                    NUM := 0 ;
                    GET ( QRD ) ;
                    TN := TN + 1 ;
                    if QRD -> in [ '0' .. '9' ] then
                      READ ( QRD , NUM ) ;
                    if TN = NUM then
                      begin
                        MAXI := 0 ;
                        ERR := IVSCAN ( MAXI , FALSE ) ;
                        if ERR <> SUCC then
                          goto 999 ;
                        if ODD ( MAXI ) then
                          MAXI := MAXI + 1 ;
                        NXT := NXT - MAXI - 2 ;
                        SB . I := NXT ;
                        for I := 1 to MAXI do
                          SB . P -> . CHR_VA [ I - 1 ] := IBF [ I ] ;
                        SB . P -> . HINT_VA [ MAXI DIV 2 ] := MAXI + 2
                                                   ;
                      end (* then *)
                    else
                      begin
                        ERR := NUMERR ;
                        goto 999
                      end (* else *) ;
                    if EOT ( QRD ) then

        (************)
        (*DO NOTHING*)
        (************)

                      ;
                  end (* while *) ;
                I := LASTPTR ;
                while I < NUMPTRS do

        (****************************)
        (* NOW PRINT EACH HEAP ITEM *)
        (****************************)

                  begin
                    I := I + 1 ;
                    NUM := ORD ( PTRS [ I ] . C ) ;
                    PTRS [ I ] . C := CHR ( 0 ) ;
                    J := PTRS [ I ] . I ;
                    if NUM <= TN then
                      begin
                        SB . I := BOTSTK -> . CURNTNP ;

        /**********************/
        /* was repeat --- ??? */
        /**********************/

                        while NUM > 0 do
                          begin
                            MAXI := SB . P -> . HINT_VA [ - 1 ] ;
                            SB . I := SB . I - MAXI ;
                            NUM := NUM - 1 ;
                          end (* while *) ;
                        PACK ( SB . P -> . CHR_VA , 0 , IBF ) ;

        (***********************************)
        (* ONLY "MAXI" CHARS NEED BE MOVED *)
        (***********************************)

                        SB . I := J ;
                        ALNFCT := BYL_INT ;
                        NUM := 1 ;
                        J := 0 ;
                        WRITELN ;
                        WRITELN ;
                        WRITE ( ' HEAP STORAGE AT ' : 22 , SB . P ) ;
                        LINELEN := 30 ;
                        PRINT ( 1 , NUM , 'H' , J , SB . P , 0 ,
                                HEAPNAME ) ;
                      end (* then *)
                    else
                      begin
                        ERR := NUMERR ;
                        MAXI := 0 ;
                        goto 999
                      end (* else *) ;
                  end (* while *) ;
                LASTPTR := I ;
              end (* then *) ;
            999 :
            if ERR <> SUCC then
              ERRMSG ( ERR , MAXI ) ;
          end (* then *)
        else
          WRITELN ( '     Procedure ' , PNAME , ' (ID:' , PID : 4 ,
                    ') not found in symbol table' ) ;
        WRITELN ;
      end (* PRINT_VARIABLE *) ;


   function GET_FROMLINE ( P : PROC_PTR ; RTNADD : INTEGER ) : INTEGER
                         ;

      const ESCAPE = 254 ;

            (************)
            (* HEX 'FE' *)
            (************)

            ESEND = 255 ;

            (************)
            (* HEX 'FF' *)
            (************)


      var NADDR , LIN , I , INDX : INTEGER ;
          TL : record
                 case INTEGER of
                   1 :
                     ( I : INTEGER ) ;
                   2 :
                     ( P : PROCTPTR )
               end ;
          OFFS : INTEGER ;
          PLEN : -> HINTEGER ;

      begin (* GET_FROMLINE *)
        OFFS := P -> . SPC1 MOD 4096 ;
        PLEN := PTRADD ( P , OFFS ) ;
        PLEN := PTRADD ( PLEN , - 6 ) ;
        I := PLEN -> ;
        if ( I <= 0 ) or ( I > PROCSIZE ) then
          GET_FROMLINE := 0
        else
          begin
            NADDR := ORD ( P ) ;
            TL . I := ORD ( P ) + I ;
            LIN := TL . P -> . SLINNO ;
            if ( RTNADD < NADDR ) or ( RTNADD >= TL . I ) then
              begin
                NADDR := 0 ;
                LIN := 0
              end (* then *) ;
            INDX := 1 ;
            with TL . P -> do
              while ( RTNADD > NADDR ) and ( ORD ( LINARY [ INDX ] ) <>
              ESEND ) do
                begin
                  I := ORD ( LINARY [ INDX ] ) ;
                  INDX := INDX + 1 ;
                  LIN := LIN + 1 ;
                  if I <> ESCAPE then
                    NADDR := NADDR + 2 * I
                  else
                    begin
                      NADDR := NADDR + ORD ( LINARY [ INDX ] ) * 512 +
                               ORD ( LINARY [ INDX + 1 ] ) * 2 ;
                      INDX := INDX + 2 ;
                    end (* else *) ;
                end (* while *) ;
            GET_FROMLINE := LIN ;
          end (* else *) ;
      end (* GET_FROMLINE *) ;


   procedure PRINT_LNK ( CURSTK , PRESTK : FRM_PTR ; var PNAME :
                       PROCNAME ; var PID , LNGTH : INTEGER ) ;

      var TPROCN : PROCNAME ;
          I , J : INTEGER ;
          DUMMYST : FRM_PTR ;
          RET24 : INTEGER ;
          EPA24 : INTEGER ;
          CALL_OFFS : INTEGER ;
          ISDEBUGP : BOOLEAN ;
          ISDEBUGC : BOOLEAN ;
          SOURCENAMEP : CHAR8 ;
          SOURCENAMEC : CHAR8 ;

      begin (* PRINT_LNK *)
        GET_PROCNAME ( CURSTK -> . EPA , PID , LNGTH , PNAME , DUMMYST
                       , ISDEBUGC , SOURCENAMEC ) ;
        GET_PROCNAME ( PRESTK -> . EPA , J , I , TPROCN , DUMMYST ,
                       ISDEBUGP , SOURCENAMEP ) ;
        WRITELN ;
        WRITELN ( '**** PROCEDURE ' , PNAME : IDLENGTH ( PNAME ) ,
                  ' WAS CALLED BY --> ' , TPROCN : IDLENGTH ( TPROCN )
                  ) ;
        if ISDEBUGC and ISDEBUGP then
          WRITELN ( '     FROM LINE: ' , GET_FROMLINE ( PRESTK -> . EPA
                    , ( CURSTK -> . RET MOD ADDRC ) ) : 1 ) ;
        WRITELN ( '     EPA address of ' , PNAME : IDLENGTH ( PNAME ) ,
                  ' is ' , CURSTK -> . EPA ) ;
        if PRESTK -> . EPA <> CURSTK -> . EPA then
          WRITELN ( '     EPA address of ' , TPROCN : IDLENGTH ( TPROCN
                    ) , ' is ' , PRESTK -> . EPA ) ;
        RET24 := CURSTK -> . RET MOD SL24 ;
        EPA24 := PTR2INT ( PRESTK -> . EPA ) MOD SL24 ;
        CALL_OFFS := RET24 - EPA24 ;
        WRITE ( '     Call offset is ' ) ;
        PRINTHEX ( CALL_OFFS , 4 ) ;
        WRITELN ;
      end (* PRINT_LNK *) ;


   procedure PRINT_SYSD ( P : ABNDP ; S : PROC_PTR ) ;

      type CODE_AT_PSW = array [ 1 .. 3 ] of VOIDPTR ;

      var I , J , K : INTEGER ;
          TPROCN : PROCNAME ;
          CODE : 0 .. 23 ;
          DUMMYST : FRM_PTR ;
          CALL_OFFS : INTEGER ;
          ISDEBUG : BOOLEAN ;
          SOURCENAME : CHAR8 ;
          ABADDR : INTEGER ;
          ABREG10 : INTEGER ;
          CP : -> CODE_AT_PSW ;

      begin (* PRINT_SYSD *)
        WRITELN ;
        WRITELN ( '**************************************************'
                  ) ;
        WRITELN ( '***   Machine and system related part          ***'
                  ) ;
        WRITELN ( '**************************************************'
                  ) ;
        WRITELN ;
        WRITELN ( '**** Interrupt PSW = ' , P -> . CHKPSW1 , ' ' , P ->
                  . CHKPSW2 ) ;
        WRITELN ( '**** Regs  0 ..  3 = ' , P -> . CHKREGS [ 0 ] , ' '
                  , P -> . CHKREGS [ 1 ] , ' ' , P -> . CHKREGS [ 2 ] ,
                  ' ' , P -> . CHKREGS [ 3 ] ) ;
        WRITELN ( '**** Regs  4 ..  7 = ' , P -> . CHKREGS [ 4 ] , ' '
                  , P -> . CHKREGS [ 5 ] , ' ' , P -> . CHKREGS [ 6 ] ,
                  ' ' , P -> . CHKREGS [ 7 ] ) ;
        WRITELN ( '**** Regs  8 .. 11 = ' , P -> . CHKREGS [ 8 ] , ' '
                  , P -> . CHKREGS [ 9 ] , ' ' , P -> . CHKREGS [ 10 ]
                  , ' ' , P -> . CHKREGS [ 11 ] ) ;
        WRITELN ( '**** Regs 12 .. 15 = ' , P -> . CHKREGS [ 12 ] , ' '
                  , P -> . CHKREGS [ 13 ] , ' ' , P -> . CHKREGS [ 14 ]
                  , ' ' , P -> . CHKREGS [ 15 ] ) ;
        CP := PTRADD ( P -> . CHKINTRP , - 6 ) ;
        WRITELN ( '**** Code at PSW   = ' , CP -> [ 1 ] , ' ' , CP -> [
                  2 ] , ' ' , CP -> [ 3 ] ) ;
        WRITELN ;
        WRITELN ( '**** ENTRY POINT $PASENT AT  : ' , P -> . CHKPASE )
                  ;
        WRITELN ( '**** BOTTOM OF RUNTIME STACK : ' , P -> . CHKREGS [
                  12 ] ) ;
        WRITELN ( '**** CURRENT STACK FRAME     : ' , P -> . CHKREGS [
                  13 ] ) ;
        WRITELN ( '**** CURRENT HEAP POINTER    : ' , P -> . CHKHEAPP )
                  ;
        WRITELN ( '**** POINTER TO TOP OF HEAP  : ' , P -> . CHKHEAPT )
                  ;
        WRITELN ;
        WRITELN ( '**************************************************'
                  ) ;
        WRITELN ( '***   Pascal specific part                     ***'
                  ) ;
        WRITELN ( '**************************************************'
                  ) ;
        WRITELN ;
        ABADDR := PTR2INT ( P -> . CHKINTRP ) ;
        ABREG10 := PTR2INT ( P -> . CHKREGS [ 10 ] ) ;
        with P -> do
          begin
            WRITE ( '**** RUN ERROR: ' , CHKERRC : 4 ) ;
            GET_PROCNAME ( S , I , J , TPROCN , DUMMYST , ISDEBUG ,
                           SOURCENAME ) ;
            CALL_OFFS := ( ABADDR MOD ADDRC ) - ( ABREG10 MOD ADDRC ) ;
            if ISDEBUG then
              WRITE ( '  FROM LINE: ' , GET_FROMLINE ( S , ABADDR MOD
                      ADDRC ) : 1 )
            else
              WRITE ( ' AT LOCATION ' , CALL_OFFS : 1 ) ;
            WRITELN ( '  OF PROCEDURE ' , TPROCN : IDLENGTH ( TPROCN )
                      ) ;
            WRITELN ( '     EPA address of ' , TPROCN : IDLENGTH (
                      TPROCN ) , ' is ' , S ) ;
            WRITE ( '     Error offset is ' ) ;
            PRINTHEX ( CALL_OFFS , 4 ) ;
            WRITELN ;
            WRITELN ;
            WRITE ( '**** ' ) ;
            if CHKERRC < 2000 then
              begin
                CODE := CHKERRC - 1000 ;
                case CODE of
                  1 : WRITE ( 'INDEX VALUE ' ) ;
                  2 : WRITE ( 'SUBRANGE VALUE ' ) ;
                  3 : WRITE ( 'ACTUAL PARAMETER ' ) ;
                  4 : WRITE ( 'SET ELEMENT(S) OUT OF RANGE ' ) ;
                  5 : WRITE ( 'POINTER VALUE ' ) ;
                  6 : WRITELN ( 'STACK/HEAP COLLISION.' ) ;
                  11 : WRITELN ( 'FILE DEFINITION ERROR.' ) ;
                  12 : WRITELN ( 'PROGRAM OUT OF STACK SPACE.' ) ;
                  13 : WRITELN ( 'CALL TO UNDEFINED STANDARD PROC.' ) ;
                  otherwise
                    WRITELN ( CHKMSG -> : CHKMSGL ) ;
                end (* case *) ;
                if ( CODE <= 3 ) or ( CODE = 5 ) then
                  begin
                    if ( CHKCVAL = UNDFINT ) or ( CHKCVAL = UNDFHINT )
                    then
                      WRITELN ( 'IS UNDEFINED.' )
                    else
                      if ( CODE = 5 ) and ( CHKCVAL = - 1 ) then
                        WRITELN ( 'IS NIL.' )
                      else
                        begin
                          WRITELN ( ' IS OUT OF RANGE.' ) ;
                          WRITELN ;
                          WRITELN ( '**** THE OFFENDING VALUE: ' ,
                                    CHKCVAL : 1 ,
                                    ' IS NOT IN THE RANGE: ' , CHKLOWR
                                    : 1 , '..' , CHKUPPR : 1 ) ;
                        end (* else *)
                  end (* then *) ;
              end (* then *)
            else
              if ( CHKERRC < 3000 ) and ( CHKERRC >= 2000 ) then
                begin
                  CODE := CHKERRC - 2000 ;
                  case CODE of
                    1 : WRITE ( 'OPERATION' ) ;
                    2 : WRITE ( 'PRIVILEGED OPERATION' ) ;
                    3 : WRITE ( 'EXECUTE' ) ;
                    4 : WRITE ( 'PROTECTION' ) ;
                    5 : WRITE ( 'ADDRESSING' ) ;
                    6 : WRITE ( 'SPECIFICATION' ) ;
                    7 : WRITE ( 'DATA' ) ;
                    8 : WRITE ( 'FIXED-POINT OVERFLOW' ) ;
                    9 : WRITE ( 'FIXED-POINT DIVIDE' ) ;
                    10 : WRITE ( 'DECIMAL OVERFLOW' ) ;
                    11 : WRITE ( 'DECIMAL DIVIDE' ) ;
                    12 : WRITE ( 'EXPONENT OVERFLOW' ) ;
                    13 : WRITE ( 'EXPONENT UNDERFLOW' ) ;
                    14 : WRITE ( 'SIGNIFICANCE' ) ;
                    15 : WRITE ( 'FLOATING-POINT DIVIDE' ) ;
                  end (* case *) ;
                  WRITELN ( ' EXCEPTION.' ) ;
                end (* then *)
              else
                if CHKERRC = 3001 then
                  WRITELN ( 'EXTERNAL ERROR: ' , CHKMSG -> : CHKMSGL )
                            ;
          end (* with *) ;
      end (* PRINT_SYSD *) ;


   begin (* $PASSNAP *)
     QRD_IS_OPEN := FALSE ;
     for CH := 'A' to 'Z' do
       VARTARY [ CH ] := UNK ;
     VARTARY [ 'A' ] := ARY ;
     VARTARY [ 'B' ] := BOL ;
     VARTARY [ 'C' ] := CHA ;
     VARTARY [ 'D' ] := REC ;
     VARTARY [ 'I' ] := INT ;
     VARTARY [ 'R' ] := RL ;
     VARTARY [ 'P' ] := PTR ;
     VARTARY [ 'S' ] := PSET ;
     VARTARY [ 'L' ] := SCL ;
     VARTARY [ 'F' ] := FIL ;
     VARTARY [ 'X' ] := PAD ;
     HEXCHARS := '0123456789ABCDEF' ;
     PROCSTOCOME := [ ] ;
     NUMPTRS := 0 ;
     LASTPTR := 0 ;
     WRITELN ;
     WRITELN ;
     WRITELN ;
     WRITELN ( '**************************************************' ) ;
     WRITELN ( '***   S N A P S H O T   D U M P                ***' ) ;
     WRITELN ( '**************************************************' ) ;
     WRITELN ;
     WRITE ( '**** SNAPSHOT WAS CALLED BY --> ' ) ;
     if LEVEL < 0 then
       begin
         WRITELN ( 'PASCAL_MONITOR' ) ;
         DMPKIND := 10 ;
         DP := DUMPPTR ;
         WRITELN ;
         WRITELN ( '**** DumpArea provided by PASCAL_MONITOR:' ) ;
         DUMPSTOR ( DUMPPTR , PTRADD ( DUMPPTR , SIZEOF ( DUMPPARM ) -
                    1 ) ) ;

     (*****************************)
     (* SAVED R1 IN RUNTIME STACK *)
     (*****************************)

         TOPSTK := DP -> . CHKREGS [ 1 ] ;

     (******************************)
     (* SAVED R12 IN RUNTIME STACK *)
     (******************************)

         BOTSTK := DP -> . CHKREGS [ 12 ] ;

     (******************************)
     (* SAVED R13 IN RUNTIME STACK *)
     (******************************)

         TMPSTK := DP -> . CHKREGS [ 13 ] ;

     (****************************)
     (* print error information  *)
     (****************************)

         ARRYLSTF := FALSE ;
         PRINT_SYSD ( DP , TMPSTK -> . EPA ) ;
         LEVEL := - 1 ;
         PAR2 := FULL ;
       end (* then *)
     else
       begin

     (******************************************)
     (* $passnap called via snapshot call from *)
     (* normal pascal procedure; 2nd parameter *)
     (* is integer and is casted here from     *)
     (* pointer parameter                      *)
     (******************************************)

         DMPKIND := PTR2INT ( DUMPPTR ) ;

     (******************************************)
     (* this is very tricky; the stack frame   *)
     (* pointers are fetched by array access   *)
     (* using certain negative indexes;        *)
     (* works only, because dmarray is the     *)
     (* very first variable (offset zero)      *)
     (******************************************)

         TOPSTK := DMARRAY [ - 24 ] ;

     (*****************************)
     (* SAVED R1 IN RUNTIME STACK *)
     (*****************************)

         BOTSTK := DMARRAY [ - 13 ] ;

     (******************************)
     (* SAVED R12 IN RUNTIME STACK *)
     (******************************)

         TMPSTK := TOPSTK -> . BAK_LNK ;
         PAR2 := DMPKIND MOD 10 ;
         ARRYLSTF := ( ( DMPKIND DIV 10 ) MOD 10 ) = 1 ;
         GET_PROCNAME ( TMPSTK -> . EPA , UID , PLEN , CPROCN , STATICP
                        , ISDEBUG , SOURCENAME ) ;
         WRITE ( CPROCN : IDLENGTH ( CPROCN ) ) ;
         if ISDEBUG then
           WRITELN ( '  FROM LINE: ' , GET_FROMLINE ( TMPSTK -> . EPA ,
                     TOPSTK -> . RET MOD ADDRC ) : 1 )
         else
           WRITELN ;
       end (* else *) ;
     DEPTH := 1 ;
     if LEVEL = - 1 then
       MAXDEPTH := DEFLEV
     else
       if LEVEL = 0 then
         MAXDEPTH := ADDRC
       else
         MAXDEPTH := LEVEL ;
     if ( PAR2 = FULL ) or ( PAR2 = STKTRACE ) then
       repeat
         DONE := ( TMPSTK = BOTSTK ) or ( DEPTH > MAXDEPTH ) ;
         if PAR2 = FULL then

     (****************************************)
     (* DUMP VARIABLES AS WELL AS CALL TRACE *)
     (****************************************)

           begin
             GET_PROCNAME ( TMPSTK -> . EPA , UID , PLEN , CPROCN ,
                            STATICP , ISDEBUG , SOURCENAME ) ;
             PRINT_VARIABLE ( TMPSTK , STATICP , CPROCN , UID , ISDEBUG
                              , SOURCENAME ) ;
           end (* then *) ;

     (*************)
     (* FULL DUMP *)
     (*************)

         if TMPSTK <> BOTSTK then
           PRINT_LNK ( TMPSTK , TMPSTK -> . BAK_LNK , CPROCN , UID ,
                       PLEN ) ;
         TMPSTK := TMPSTK -> . BAK_LNK ;
         DEPTH := DEPTH + 1 ;
       until DONE ;
     if NUMPTRS = MAXPTRS then
       WRITELN ( '**** NOTE: NOT ALL HEAP STORAGE ITEMS WERE PRINTED.'
                 ) ;
     WRITELN ;
     WRITELN ( '**** END OF SNAPSHOT DUMP ****' ) ;
     WRITELN ;
   end (* $PASSNAP *) ;



begin (* HAUPTPROGRAMM *)

end (* HAUPTPROGRAMM *) .
