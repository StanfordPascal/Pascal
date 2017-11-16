program ISO7185PAT ( OUTPUT ) ;

/********************************************************************/
/*                                                                  */
/*                  TEST SUITE FOR ISO 7185 PASCAL                  */
/*                                                                  */
/*                   The "PASCAL ACCEPTANCE TEST"                   */
/*                                                                  */
/*                          Version 1.1                             */
/*                                                                  */
/*        Copyright (C) 2010 S. A. Moore - All rights reserved      */
/*                                                                  */
/*   This program attempts to use and display the results of each   */
/*   feature of standard pascal. It is a "positive" test in that    */
/*   it should compile and run error free, and thus does not        */
/*   check error conditions/detection.                              */
/*                                                                  */
/*   Each test is labeled and numbered, and the expected result     */
/*   also output, so that the output can be self evidently          */
/*   hand checked.                                                  */
/*                                                                  */
/*   The output can be redirected to a printer or a file to         */
/*   facilitate such checking.                                      */
/*                                                                  */
/*   The output can also be automatically checked by comparing      */
/*   a known good file to the generated file. To this end, we       */
/*   have regularized the output, specifying all output field       */
/*   widths that are normally compiler dependent.                   */
/*                                                                  */
/*   Only the following factors exist that are allowed to be        */
/*   compiler dependent, but could cause a miscompare of the        */
/*   output:                                                        */
/*                                                                  */
/*      1. The case of output booleans. We have choosen a           */
/*         standard format of LOWER case for such output.           */
/*         Note that compilers can choose any case,                 */
/*         or mixture of cases.                                     */
/*                                                                  */
/*   Because of this, it may be required to return to hand          */
/*   checking when encountering a differing compiler system.        */
/*                                                                  */
/*   Notes:                                                         */
/*                                                                  */
/*   1. This test will not run or compile unless "set of char"      */
/*   is possible. This does not mean that compilers lacking         */
/*   in "set of char" capability are not standard. However,         */
/*   in the authors opinion, this is a crippling limitation         */
/*   for any Pascal compiler.                                       */
/*                                                                  */
/*   2. Because there is no "close" function in ISO 7185            */
/*   Pascal, the file handling contained with is likely             */
/*   to generate a large number of open temporary files.            */
/*   This may cause some implementations to trip a limit            */
/*   on the number of total open files. If this occurs,             */
/*   turn the constant "testfile" below to "false".                 */
/*   This will cause the temporary files test to be skipped.        */
/*                                                                  */
/*   The following sections need to be completed:                   */
/*                                                                  */
/*   1. Buffer variables. The full suite of handing tests           */
/*   need to be applied to file buffer variables as well.           */
/*   This means all integer, character, boolean, etc.               */
/*                                                                  */
/*   2. Arrays, records and pointers containing files.              */
/*                                                                  */
/*   3. Pointer variables, array variables, and other complex       */
/*   accesses need to subjected to the same extentive tests         */
/*   that base variables are.                                       */
/*                                                                  */
/*   4. Need a test for access to locals of a surrounding           */
/*   procedure. This tests access to a procedure that is local,     */
/*   but not in the same scope.                                     */
/*                                                                  */
/*   5. Need a dynamic storage test that allocates various          */
/*   sizes, not just integers.                                      */
/*                                                                  */
/*   6. The tests that test BSI equivalents, but don't yet          */
/*   pass P5 are labeled "*** BSI ***", and commented out.          */
/*   These need to be fixed.                                        */
/*                                                                  */
/********************************************************************/


label 1 , 2 , 3 ;


const TCNST = 768 ;
      SCST = 'this is a string' ;
      CCST = 'v' ;
      TSNCST = - 52 ;
      RCNST = 43.33 ;
      RSCST = - 84.22 ;
      TSNCST2 = - TCNST ;
      TSNCST3 = - TSNCST ;
      RSCST2 = - RCNST ;
      RSCST3 = - RSCST ;
      TESTFILE = TRUE ;
      MMAXINT = - MAXINT ;


type STRING10 = packed array [ 1 .. 10 ] of CHAR ;
     ENUM = ( ONE , TWO , THREE , FOUR , FIVE , SIX , SEVEN , EIGHT ,
            NINE , TEN ) ;
     ESUB = THREE .. SIX ;
     SUBR = 10 .. 20 ;

     (****************************)
     (* note use of alternatives *)
     (****************************)

     ARRI = array [ 1 .. 10 ] of INTEGER ;
     ARRIM = array [ 1 .. 2 , 1 .. 2 ] of array [ 1 .. 2 , 1 .. 2 , 1
             .. 2 , 1 .. 2 ] of INTEGER ;
     CSET = set of CHAR ;
     IPTR = -> INTEGER ;
     RECS = record
              A : INTEGER ;
              B : CHAR
            end ;
     REC = record
             I : INTEGER ;
             B : BOOLEAN ;
             C : CHAR ;
             E : ENUM ;
             ES : ESUB ;
             S : SUBR ;
             R : REAL ;
             ST : STRING10 ;
             A : ARRI ;
             RC : RECS ;
             STC : CSET ;
             P : IPTR
           end ;
     PREC = packed record
                     I : INTEGER ;
                     B : BOOLEAN ;
                     C : CHAR ;
                     E : ENUM ;
                     ES : ESUB ;
                     S : SUBR ;
                     R : REAL ;
                     ST : STRING10 ;
                     A : ARRI ;
                     RC : RECS ;
                     STC : CSET ;
                     P : IPTR
                   end ;
     RECV = record
              A : INTEGER ;
              B : CHAR ;
              case C : BOOLEAN of
                FALSE :
                  ( D : STRING10 ) ;
                TRUE :
                  ( E : ENUM )

     (********)
     (* end  *)
     (********)

            end ;
     ARRR = array [ 1 .. 10 ] of RECS ;
     VART = ( VTI , VTB , VTC , VTE , VTES , VTS , VTR , VTST , VTA ,
            VTRC , VTSTC , VTP ) ;
     INTALIAS = INTEGER ;


var I , X , Y , Z , Q , N , T : INTEGER ;
    SRX , SRY , SRZ : 0 .. 100 ;
    SRAS , SRBS , SRCS , SRDS , SRES : - 100 .. 100 ;
    A : array [ 1 .. 10 ] of INTEGER ;
    R : record
          RX : INTEGER ;
          RC : CHAR ;
          RY : INTEGER ;
          RB : BOOLEAN ;
          RS : packed array [ 1 .. 10 ] of CHAR ;
        end ;
    DA : array [ 1 .. 10 , 1 .. 10 ] of INTEGER ;
    SA , SB , SC : packed array [ 1 .. 10 ] of CHAR ;
    CA , CB , CC : CHAR ;
    CAR : array [ 'a' .. 'z' ] of INTEGER ;
    SAR : array [ 1 .. 10 ] of packed array [ 1 .. 10 ] of CHAR ;
    BA , BB , BC : BOOLEAN ;
    SVA , SVB , SVC : ( MON , TUE , WED , THUR , FRI , SAT , SUN ) ;
    S : STRING10 ;
    AS , BS , CS , DS , ES , GS , HS : INTEGER ;
    VNUM : - MAXINT .. MAXINT ;
    RA , RB , RC , RD , RE : REAL ;
    STA , STB , STC , STD : set of 1 .. 100 ;
    STE : set of 1 .. 10 ;
    STF : packed set of 1 .. 10 ;
    STG : packed set of 1 .. 20 ;
    CSTA , CSTB , CSTC , CSTD : set of CHAR ;
    CSTE : set of 'a' .. 'z' ;
    CSTF : packed set of 'a' .. 'f' ;
    CSTG : packed set of CHAR ;
    CI : CHAR ;
    SENA , SENB , SENC , SEND : set of ENUM ;
    SENE : set of ONE .. FIVE ;
    SENF : packed set of ENUM ;
    SENG : packed set of ONE .. SEVEN ;
    EI , EA : ENUM ;
    SBA , SBB , SBC , SBD : set of BOOLEAN ;
    SBE : set of FALSE .. TRUE ;
    SBF : packed set of BOOLEAN ;
    SBG : packed set of FALSE .. TRUE ;
    AI : ARRI ;
    AREC : REC ;
    PAREC : PREC ;
    VREC : RECV ;
    IP : IPTR ;
    AVI : ARRI ;
    AVI2 : ARRI ;
    PAVI : packed array [ 1 .. 10 ] of INTEGER ;
    AVIS : array [ 1 .. 10 ] of 10 .. 20 ;
    PAVIS : packed array [ 1 .. 10 ] of 10 .. 20 ;
    AVB : array [ 1 .. 10 ] of BOOLEAN ;
    PAVB : packed array [ 1 .. 10 ] of BOOLEAN ;
    AVR : array [ 1 .. 10 ] of REAL ;
    PAVR : packed array [ 1 .. 10 ] of REAL ;
    AVC : array [ 1 .. 10 ] of CHAR ;
    PAVC : packed array [ 1 .. 10 ] of CHAR ;
    AVCS : array [ 1 .. 10 ] of 'g' .. 'p' ;
    PAVCS : packed array [ 1 .. 10 ] of 'g' .. 'p' ;
    AVE : array [ 1 .. 10 ] of ENUM ;
    PAVE : packed array [ 1 .. 10 ] of ENUM ;
    AVES : array [ 1 .. 10 ] of ESUB ;
    PAVES : packed array [ 1 .. 10 ] of ESUB ;
    AVS : array [ 1 .. 10 ] of CSET ;
    PAVS : packed array [ 1 .. 10 ] of CSET ;
    AVRC : array [ 1 .. 10 ] of RECS ;
    PAVRC : packed array [ 1 .. 10 ] of RECS ;
    AVF : array [ 1 .. 10 ] of TEXT ;
    PAVF : packed array [ 1 .. 10 ] of TEXT ;
    AVP : array [ 1 .. 10 ] of IPTR ;
    PAVP : packed array [ 1 .. 10 ] of IPTR ;
    BIA : array [ BOOLEAN ] of INTEGER ;
    PBIA : packed array [ BOOLEAN ] of INTEGER ;
    CIA : array [ CHAR ] of INTEGER ;
    PCIA : packed array [ CHAR ] of INTEGER ;
    CSIA : array [ 'a' .. 'z' ] of INTEGER ;
    PCSIA : packed array [ 'a' .. 'z' ] of INTEGER ;
    EIA : array [ ENUM ] of INTEGER ;
    PEIA : packed array [ ENUM ] of INTEGER ;
    ESIA : array [ TWO .. SIX ] of INTEGER ;
    PESIA : packed array [ TWO .. SIX ] of INTEGER ;
    MDAR : ARRIM ;
    MDAR2 : ARRIM ;
    VRA : record
            I : INTEGER ;
            case VT : VART of
              VTI :
                ( VDI : INTEGER ;
                  A : INTEGER ) ;
              VTB :
                ( VDB : BOOLEAN ;
                  B : INTEGER ) ;
              VTC :
                ( VDC : CHAR ;
                  C : INTEGER ) ;
              VTE :
                ( VDE : ENUM ;
                  D : INTEGER ) ;
              VTES :
                ( VDES : ESUB ;
                  E : INTEGER ) ;
              VTS :
                ( VDS : SUBR ;
                  F : INTEGER ) ;
              VTR :
                ( VDR : REAL ;
                  G : INTEGER ) ;
              VTST :
                ( VDST : STRING10 ;
                  H : INTEGER ) ;
              VTA :
                ( VDA : ARRI ;
                  J : INTEGER ) ;
              VTRC :
                ( VDRC : RECS ;
                  K : INTEGER ) ;
              VTSTC :
                ( VDSTC : CSET ;
                  L : INTEGER ) ;
              VTP :
                ( VDP : IPTR ;
                  M : INTEGER )

    (********)
    (* end  *)
    (********)

          end ;
    VVRS : record
             case VT : SUBR of
               10 , 11 , 12 , 13 , 14 , 15 :
                 ( VI : INTEGER ) ;
               16 , 17 , 18 , 19 , 20 :
                 ( VB : BOOLEAN )

    (********)
    (* end  *)
    (********)

           end ;
    VVRB : record
             case VT : BOOLEAN of
               TRUE :
                 ( VI : INTEGER ) ;
               FALSE :
                 ( VB : BOOLEAN )

    (********)
    (* end  *)
    (********)

           end ;
    VVRE : record
             case VT : ENUM of
               ONE , TWO , THREE , FOUR , FIVE :
                 ( VI : INTEGER ) ;
               SIX , SEVEN , EIGHT , NINE , TEN :
                 ( VB : BOOLEAN )

    (********)
    (* end  *)
    (********)

           end ;
    VVRES : record
              case VT : ESUB of
                THREE , FOUR :
                  ( VI : INTEGER ) ;
                FIVE , SIX :
                  ( VB : BOOLEAN )

    (********)
    (* end  *)
    (********)

            end ;
    NVR : record
            I : INTEGER ;
            R : record
                  I : INTEGER ;
                  R : record
                        I : INTEGER ;
                        R : record
                              I : INTEGER ;
                              R : record
                                    I : INTEGER ;
                                    R : record
                                          I : INTEGER ;
                                          R : record
                                                I : INTEGER ;
                                                R : record
                                                   I : INTEGER ;
                                                   R : record
                                                   I : INTEGER ;
                                                   R : record
                                                   I : INTEGER
                                                   end
                                                   end
                                                   end
                                              end
                                        end
                                  end
                            end
                      end
                end
          end ;
    RPA : -> REC ;
    ARA : ARRR ;
    FI : FILE of INTEGER ;
    PFI : packed FILE of INTEGER ;
    FB : FILE of BOOLEAN ;
    PFB : packed FILE of BOOLEAN ;
    FC : FILE of CHAR ;
    PFC : packed FILE of CHAR ;
    FE : FILE of ENUM ;
    PFE : packed FILE of ENUM ;
    FES : FILE of ESUB ;
    PFES : packed FILE of ESUB ;
    FS : FILE of SUBR ;
    PFS : packed FILE of SUBR ;
    FR : FILE of REAL ;
    PFR : packed FILE of REAL ;
    FST : FILE of STRING10 ;
    PFST : packed FILE of STRING10 ;
    FA : FILE of ARRI ;
    PFA : packed FILE of ARRI ;
    FRC : FILE of RECS ;
    PFRC : packed FILE of RECS ;
    FSTC : FILE of CSET ;
    PFSTC : packed FILE of CSET ;
    FP : FILE of IPTR ;
    PFP : packed FILE of IPTR ;
    FT : TEXT ;
    PTI , PTI1 : -> INTEGER ;
    PTB : -> BOOLEAN ;
    PTC : -> CHAR ;
    PTE : -> ENUM ;
    PTES : -> ESUB ;
    PTS : -> SUBR ;
    PTR : -> REAL ;
    PTST : -> STRING10 ;
    PTA : -> ARRI ;
    PTRC : -> RECS ;
    PTSTC : -> CSET ;
    PTP : -> IPTR ;
    IPA , IPB , IPC , IPD , IPE : -> INTEGER ;
    IAP : array [ 1 .. 100 ] of -> INTEGER ;
    RNDSEQ : INTEGER ;
    CNT , CNT2 : INTEGER ;
    RN : INTEGER ;
    RCASTT : INTEGER ;
    RCAST : record
              case RCASTT : BOOLEAN of
                TRUE :
                  ( ) ;
                FALSE :
                  ( )
            end ;
    PI1 , PI2 : -> INTEGER ;



procedure JUNK1 ( Z , Q : INTEGER ) ;

   begin (* JUNK1 *)
     WRITE ( Z : 1 , ' ' , Q : 1 ) ;
   end (* JUNK1 *) ;



procedure JUNK2 ( var Z : INTEGER ) ;

   begin (* JUNK2 *)
     Z := Z + 1
   end (* JUNK2 *) ;



procedure JUNK3 ( var P : STRING10 ) ;

   begin (* JUNK3 *)
     WRITE ( P )
   end (* JUNK3 *) ;



procedure JUNK4 ( P : STRING10 ) ;

   begin (* JUNK4 *)
     P [ 5 ] := '?' ;
     WRITE ( P )
   end (* JUNK4 *) ;



function JUNK5 ( X : INTEGER ) : INTEGER ;

   begin (* JUNK5 *)
     JUNK5 := X + 1
   end (* JUNK5 *) ;



procedure JUNK6 ;

   begin (* JUNK6 *)
     goto 2
   end (* JUNK6 *) ;



function JUNK7 ( A , B , C : INTEGER ) : INTEGER ;

   FORWARD ;



function JUNK7 ;

   var X , Y , Z : INTEGER ;

   begin (* JUNK7 *)
     X := 1 ;
     Y := 2 ;
     Z := 3 ;
     WRITE ( A : 1 , ' ' , B : 1 , ' ' , C : 1 , ' ' ) ;
     A := 4 ;
     B := 5 ;
     C := 6 ;
     WRITE ( C : 1 , ' ' , B : 1 , ' ' , A : 1 , ' ' , Z : 1 , ' ' , Y
             : 1 , ' ' , X : 1 ) ;
     JUNK7 := 78
   end (* JUNK7 *) ;



procedure JUNK8 ( A : INTEGER ; B : BOOLEAN ; C : CHAR ; E : ENUM ; ES
                : ESUB ; S : SUBR ; R : REAL ; ST : STRING10 ; AR :
                ARRI ; RC : REC ; RV : RECV ; STC : CSET ; P : IPTR ) ;

   var I : INTEGER ;
       CI : CHAR ;

   begin (* JUNK8 *)
     WRITELN ( A : 1 , ' ' , B : 5 , ' ' , C : 1 , ' ' , ORD ( E ) : 1
               , ' ' , ORD ( ES ) : 1 , ' ' , S : 1 , ' ' , R : 15 ,
               ' ' , ST ) ;
     for I := 1 to 10 do
       WRITE ( AR [ I ] : 1 , ' ' ) ;
     WRITELN ;
     WRITELN ( RC . I : 1 , ' ' , RC . B : 5 , ' ' , RC . C : 1 , ' ' ,
               ORD ( RC . E ) : 1 , ' ' , ORD ( RC . ES ) : 1 , ' ' ,
               RC . S : 1 , ' ' , RC . R : 15 , ' ' , RC . ST ) ;
     for I := 1 to 10 do
       WRITE ( RC . A [ I ] : 1 , ' ' ) ;
     WRITELN ;
     WRITELN ( RC . RC . A : 1 , ' ' , RC . RC . B : 1 ) ;
     for CI := 'a' to 'j' do
       if CI in RC . STC then
         WRITE ( CI )
       else
         WRITE ( '_' ) ;
     WRITELN ;
     WRITELN ( RC . P -> : 1 ) ;
     WRITELN ( RV . A : 1 , ' ' , RV . B : 1 , ' ' , RV . C : 5 ) ;
     if RV . C then
       WRITELN ( ORD ( RV . E ) : 1 )
     else
       WRITELN ( RV . D ) ;
     for CI := 'a' to 'j' do
       if CI in STC then
         WRITE ( CI )
       else
         WRITE ( '_' ) ;
     WRITELN ;
     WRITELN ( P -> : 1 )
   end (* JUNK8 *) ;



procedure JUNK9 ( procedure JUNK9 ( JUNK9 , B : INTEGER ; C : CHAR ) ;
                function Y ( A : INTEGER ) : INTEGER ) ;

   begin (* JUNK9 *)
     JUNK9 ( 9834 , 8383 , 'j' ) ;
     WRITE ( ' ' , Y ( 743 ) : 1 ) ;
   end (* JUNK9 *) ;



procedure JUNK10 ( X , Y : INTEGER ; JUNK10 : CHAR ) ;

   begin (* JUNK10 *)
     WRITE ( X : 1 , ' ' , Y : 1 , ' ' , JUNK10 : 1 )
   end (* JUNK10 *) ;



function JUNK11 ( X : INTEGER ) : INTEGER ;

   begin (* JUNK11 *)
     JUNK11 := SUCC ( X )
   end (* JUNK11 *) ;



procedure JUNK12 ( procedure XQ ( function YQ ( Z : INTEGER ) : INTEGER
                 ) ; function Q ( N : INTEGER ) : INTEGER ) ;

   begin (* JUNK12 *)
     XQ ( Q )
   end (* JUNK12 *) ;



procedure JUNK13 ( function XZ ( Z : INTEGER ) : INTEGER ) ;

   begin (* JUNK13 *)
     WRITE ( XZ ( 941 ) : 1 )
   end (* JUNK13 *) ;



procedure JUNK14 ;

   var I , X : INTEGER ;


   procedure JUNK15 ;

      begin (* JUNK15 *)
        WRITE ( I : 1 , ' ' , X : 1 )
      end (* JUNK15 *) ;


   begin (* JUNK14 *)
     I := 62 ;
     X := 76 ;
     JUNK15
   end (* JUNK14 *) ;



procedure JUNK16 ;

   begin (* JUNK16 *)

   end (* JUNK16 *) ;



procedure JUNK17 ( procedure X ; I : INTEGER ) ;


   procedure JUNK18 ;

      begin (* JUNK18 *)
        WRITE ( I : 1 )
      end (* JUNK18 *) ;


   begin (* JUNK17 *)
     X ;
     if I = 52 then
       JUNK17 ( JUNK18 , 83 )
   end (* JUNK17 *) ;





(*******************************************************)
(* test preference of pointer bonding to current scope *)
(*******************************************************)




procedure JUNK19 ;

   type PT = -> INTALIAS ;
        INTALIAS = CHAR ;

   var P : PT ;

   begin (* JUNK19 *)
     NEW ( P ) ;
     P -> := 'a' ;
     WRITE ( P -> ) ;
     DISPOSE ( P )
   end (* JUNK19 *) ;





(*************************************************************)
(* test ability to assign function result to nested function *)
(*************************************************************)




function JUNK20 : INTEGER ;


   function INNER : INTEGER ;

      begin (* INNER *)
        INNER := 12 ;
        JUNK20 := 37
      end (* INNER *) ;


   begin (* JUNK20 *)
     I := INNER
   end (* JUNK20 *) ;



function RANDOM ( LOW , HI : INTEGER ) : INTEGER ;

   const A = 16807 ;
         M = 2147483647 ;

   var GAMMA : INTEGER ;

   begin (* RANDOM *)
     GAMMA := A * ( RNDSEQ MOD ( M DIV A ) ) - ( M MOD A ) * ( RNDSEQ
              DIV ( M DIV A ) ) ;
     if GAMMA > 0 then
       RNDSEQ := GAMMA
     else
       RNDSEQ := GAMMA + M ;
     RANDOM := RNDSEQ DIV ( MAXINT DIV ( HI - LOW + 1 ) ) + LOW
   end (* RANDOM *) ;



begin (* HAUPTPROGRAMM *)
  WRITE (
    '****************************************************************'
          ) ;
  WRITELN ( '***************' ) ;
  WRITELN ;
  WRITELN ( '                 TEST SUITE FOR ISO 7185 PASCAL' ) ;
  WRITELN ;
  WRITE (
       '                 Copyright (C) 1995 S. A. Moore - All rights '
          ) ;
  WRITELN ( 'reserved' ) ;
  WRITELN ;
  WRITE (
    '****************************************************************'
          ) ;
  WRITELN ( '***************' ) ;
  WRITELN ;

  (*****************************************************************)
  (*                                                               *)
  (*                              Metering                         *)
  (*                                                               *)
  (*****************************************************************)

  WRITELN ( 'The following are implementation defined characteristics'
            ) ;
  WRITELN ;
  WRITELN ( 'Maxint: ' , MAXINT : 1 ) ;
  I := MAXINT ;
  X := 0 ;
  while I > 0 do
    begin
      I := I DIV 2 ;
      X := X + 1
    end (* while *) ;
  WRITELN ( 'Bit length of integer without sign bit appears to be: ' ,
            X : 1 ) ;
  WRITELN ( 'Integer default output field' ) ;
  WRITELN ( '         1111111111222222222233333333334' ) ;
  WRITELN ( '1234567890123456789012345678901234567890' ) ;
  WRITELN ( 1 ) ;
  WRITELN ( 'Real default output field' ) ;
  WRITELN ( '         1111111111222222222233333333334' ) ;
  WRITELN ( '1234567890123456789012345678901234567890' ) ;
  WRITELN ( 1.2 ) ;
  WRITELN ( 'Boolean default output field' ) ;
  WRITELN ( '         1111111111222222222233333333334' ) ;
  WRITELN ( '1234567890123456789012345678901234567890' ) ;
  WRITELN ( FALSE ) ;
  WRITELN ( TRUE ) ;
  WRITELN ( 'Char default output field' ) ;
  WRITELN ( '         1111111111222222222233333333334' ) ;
  WRITELN ( '1234567890123456789012345678901234567890' ) ;
  WRITELN ( 'a' ) ;
  if ( ORD ( 'a' ) = 97 ) and ( ORD ( '(' ) = 40 ) and ( ORD ( '^' ) =
  94 ) then
    WRITELN ( 'Appears to be ASCII' )
  else
    WRITELN ( 'Appears to not be ASCII' ) ;

  (*****************************************************************)
  (*                                                               *)
  (*                        Control structures                     *)
  (*                                                               *)
  (*****************************************************************)

  WRITELN ;
  WRITELN (
    '******************* Control structures tests *******************'
            ) ;
  WRITELN ;
  WRITE ( 'Control1: ' ) ;
  for I := 1 to 10 do
    WRITE ( I : 1 , ' ' ) ;
  WRITELN ( 's/b 1 2 3 4 5 6 7 8 9 10' ) ;
  WRITE ( 'Control2: ' ) ;
  for I := 10 DOWNTO 1 do
    WRITE ( I : 1 , ' ' ) ;
  WRITELN ( 's/b 10 9 8 7 6 5 4 3 2 1' ) ;
  WRITE ( 'Control3: ' ) ;
  I := 1 ;
  while I <= 10 do
    begin
      WRITE ( I : 1 , ' ' ) ;
      I := I + 1
    end (* while *) ;
  WRITELN ( 's/b 1 2 3 4 5 6 7 8 9 10' ) ;
  WRITE ( 'Control4: ' ) ;
  I := 1 ;
  repeat
    WRITE ( I : 1 , ' ' ) ;
    I := I + 1
  until I > 10 ;
  WRITELN ( 's/b 1 2 3 4 5 6 7 8 9 10' ) ;
  WRITE ( 'Control5: ' ) ;
  I := 1 ;
  1 :
  WRITE ( I : 1 , ' ' ) ;
  I := I + 1 ;
  if I <= 10 then
    goto 1 ;
  WRITELN ( 's/b 1 2 3 4 5 6 7 8 9 10' ) ;
  WRITE ( 'Control6: ' ) ;
  if TRUE then
    WRITE ( 'yes' )
  else
    WRITE ( 'no' ) ;
  WRITELN ( ' s/b yes' ) ;
  WRITE ( 'Control7: ' ) ;
  if FALSE then
    WRITE ( 'no' )
  else
    WRITE ( 'yes' ) ;
  WRITELN ( ' s/b yes' ) ;
  WRITE ( 'Control8: ' ) ;
  if TRUE then
    WRITE ( 'yes ' ) ;
  WRITE ( 'stop' ) ;
  WRITELN ( ' s/b yes stop' ) ;
  WRITE ( 'Control9: ' ) ;
  if FALSE then
    WRITE ( 'no ' ) ;
  WRITE ( 'stop' ) ;
  WRITELN ( ' s/b stop' ) ;
  WRITE ( 'Control10: ' ) ;
  for I := 1 to 10 do
    case I of
      1 : WRITE ( 'one ' ) ;
      2 : WRITE ( 'two ' ) ;
      3 : WRITE ( 'three ' ) ;
      4 : WRITE ( 'four ' ) ;
      5 : WRITE ( 'five ' ) ;
      6 : WRITE ( 'six ' ) ;
      7 : WRITE ( 'seven ' ) ;
      8 : WRITE ( 'eight ' ) ;
      9 , 10 :
        WRITE ( 'nine-ten ' )
    end (* case *) ;
  WRITELN ;
  WRITE ( 'Control10: s/b ' ) ;
  WRITE ( 'one two three four five ' ) ;
  WRITELN ( 'six seven eight nine-ten nine-ten' ) ;
  WRITE ( 'Control11: start ' ) ;
  JUNK6 ;
  WRITE ( '!! BAD !!' ) ;
  2 :
  WRITELN ( 'stop s/b start stop' ) ;
  WRITE ( 'Control12: start ' ) ;
  goto 003 ;
  WRITE ( '!! BAD !!' ) ;
  3 :
  WRITELN ( 'stop s/b start stop' ) ;
  WRITE ( 'Control13: start ' ) ;

  (*********************)
  (* self defined fors *)
  (*********************)

  I := 10 ;
  for I := 1 to I do
    WRITE ( I : 3 ) ;
  WRITELN ( ' s/b   1  2  3  4  5  6  7  8  9 10' ) ;
  WRITE ( 'Control14: start ' ) ;

  (*********************)
  (* self defined fors *)
  (*********************)

  I := 10 ;
  for I := I DOWNTO 1 do
    WRITE ( I : 3 ) ;
  WRITELN ( ' s/b  10  9  8  7  6  5  4  3  2  1' ) ;
  WRITE ( 'Control15: start ' ) ;

  (*****************)
  (* for against 0 *)
  (*****************)

  for I := 0 to 9 do
    WRITE ( I : 2 ) ;
  WRITELN ( ' s/b 0 1 2 3 4 5 6 7 8 9' ) ;
  WRITE ( 'Control16: start ' ) ;

  (*****************)
  (* for against 0 *)
  (*****************)

  for I := 9 DOWNTO 0 do
    WRITE ( I : 2 ) ;
  WRITELN ( ' s/b 9 8 7 6 5 4 3 2 1 0' ) ;

  (**********************************)
  (* wide spread of case statements *)
  (**********************************)

  WRITE ( 'Control17: start ' ) ;
  I := 10000 ;
  case I of
    1 : WRITE ( '*** bad ***' ) ;
    10000 : WRITE ( 'good' )
  end (* case *) ;
  WRITELN ( ' start s/b good' ) ;

  (*****************************************************************)
  (*                                                               *)
  (*                         Integers                              *)
  (*                                                               *)
  (*****************************************************************)

  WRITELN ;
  WRITELN ( '******************* Integers *******************' ) ;
  WRITELN ;

  (*********************)
  (* integer variables *)
  (*********************)

  X := 43 ;
  Y := 78 ;
  Z := Y ;
  WRITELN ( 'Integer1:   ' , X + Y : 1 , ' s/b 121' ) ;
  WRITELN ( 'Integer2:   ' , Y - X : 1 , ' s/b 35' ) ;
  WRITELN ( 'Integer3:   ' , X * Y : 1 , ' s/b 3354' ) ;
  WRITELN ( 'Integer4:   ' , Y DIV X : 1 , ' s/b 1' ) ;
  WRITELN ( 'Integer5:   ' , Y MOD X : 1 , ' s/b 35' ) ;
  WRITELN ( 'Integer6:   ' , SUCC ( X ) : 1 , ' s/b 44' ) ;
  WRITELN ( 'Integer7:   ' , PRED ( X ) : 1 , ' s/b 42' ) ;
  WRITELN ( 'Integer8:   ' , SQR ( X ) : 1 , ' s/b 1849' ) ;
  WRITELN ( 'Integer9:   ' , CHR ( Y ) , ' s/b N' ) ;
  WRITELN ( 'Integer10:  ' , ORD ( CHR ( X ) ) : 1 , ' s/b 43' ) ;
  WRITELN ( 'Integer11:  ' , ODD ( X ) : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer12:  ' , ODD ( Y ) : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer13:  ' , Z = Y : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer14:  ' , X = Y : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer15:  ' , X < Y : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer16:  ' , Y < X : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer17:  ' , Y > X : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer18:  ' , X > Y : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer19:  ' , X <> Y : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer20:  ' , Y <> Z : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer21:  ' , X <= Y : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer22:  ' , Z <= Y : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer23:  ' , Y <= X : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer24:  ' , Y >= X : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer25:  ' , Y >= Z : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer26:  ' , X >= Y : 5 , ' s/b false' ) ;

  (******************************)
  (* unsigned integer constants *)
  (******************************)

  WRITE ( 'Integer27:  ' ) ;
  I := 546 ;
  WRITELN ( I : 1 , ' s/b 546' ) ;
  WRITELN ( 'Integer28:  ' , 56 + 34 : 1 , ' s/b 90' ) ;
  WRITELN ( 'Integer29:  ' , 56 - 34 : 1 , ' s/b 22' ) ;
  WRITELN ( 'Integer30:  ' , 56 * 34 : 1 , ' s/b 1904' ) ;
  WRITELN ( 'Integer31:  ' , 56 DIV 34 : 1 , ' s/b 1' ) ;
  WRITELN ( 'Integer32:  ' , 56 MOD 34 : 1 , ' s/b 22' ) ;
  WRITELN ( 'Integer33:  ' , SUCC ( 5 ) : 1 , ' s/b 6' ) ;
  WRITELN ( 'Integer34:  ' , PRED ( 5 ) : 1 , ' s/b 4' ) ;
  WRITELN ( 'Integer35:  ' , SQR ( 7 ) : 1 , ' s/b 49' ) ;
  WRITELN ( 'Integer36:  ' , CHR ( 65 ) , ' s/b A' ) ;
  WRITELN ( 'Integer37:  ' , ORD ( CHR ( 65 ) ) : 1 , ' s/b 65' ) ;
  WRITELN ( 'Integer38:  ' , TCNST : 1 , ' s/b 768' ) ;
  WRITELN ( 'Integer39:  ' , ODD ( 5 ) : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer40:  ' , ODD ( 8 ) : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer41:  ' , 56 = 56 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer42:  ' , 56 = 57 : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer43:  ' , 56 < 57 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer44:  ' , 57 < 56 : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer45:  ' , 57 > 56 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer46:  ' , 56 > 57 : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer47:  ' , 56 <> 57 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer48:  ' , 56 <> 56 : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer49:  ' , 55 <= 500 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer50:  ' , 67 <= 67 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer51:  ' , 56 <= 33 : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer52:  ' , 645 >= 4 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer53:  ' , 23 >= 23 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer54:  ' , 45 >= 123 : 5 , ' s/b false' ) ;

  (****************************)
  (* signed integer variables *)
  (****************************)

  AS := - 14 ;
  BS := - 32 ;
  CS := - 14 ;
  DS := 20 ;
  ES := - 15 ;
  GS := MAXINT ;
  HS := MMAXINT ;
  VNUM := - MAXINT ;
  WRITELN ( 'Integer55:  ' , AS + DS : 1 , ' s/b 6' ) ;
  WRITELN ( 'Integer56:  ' , DS + AS : 1 , ' s/b 6' ) ;
  WRITELN ( 'Integer57:  ' , BS + DS : 1 , ' s/b -12' ) ;
  WRITELN ( 'Integer58:  ' , AS + BS : 1 , ' s/b -46' ) ;
  WRITELN ( 'Integer59:  ' , DS - AS : 1 , ' s/b 34' ) ;
  WRITELN ( 'Integer60:  ' , BS - DS : 1 , ' s/b -52' ) ;
  WRITELN ( 'Integer61:  ' , BS - AS : 1 , ' s/b -18' ) ;
  WRITELN ( 'Integer62:  ' , DS * AS : 1 , ' s/b -280' ) ;
  WRITELN ( 'Integer63:  ' , AS * DS : 1 , ' s/b -280' ) ;
  WRITELN ( 'Integer64:  ' , AS * BS : 1 , ' s/b 448' ) ;
  WRITELN ( 'Integer65:  ' , DS DIV AS : 1 , ' s/b -1' ) ;
  WRITELN ( 'Integer66:  ' , BS DIV DS : 1 , ' s/b -1' ) ;
  WRITELN ( 'Integer67:  ' , BS DIV AS : 1 , ' s/b 2' ) ;
  WRITELN ( 'Integer68:  ' , SUCC ( AS ) : 1 , ' s/b -13' ) ;
  WRITELN ( 'Integer69:  ' , PRED ( BS ) : 1 , ' s/b -33' ) ;
  WRITELN ( 'Integer70: ' , SQR ( AS ) : 1 , ' s/b 196' ) ;
  WRITELN ( 'Integer71:  ' , ODD ( AS ) : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer72:  ' , ODD ( ES ) : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer73:  ' , AS = CS : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer74:  ' , AS = BS : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer75:  ' , AS <> BS : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer76:  ' , AS <> CS : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer77:  ' , AS < DS : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer78:  ' , BS < AS : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer79:  ' , DS < AS : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer80:  ' , AS < BS : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer81:  ' , DS > AS : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer82:  ' , AS > BS : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer83:  ' , AS > DS : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer84:  ' , BS > AS : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer85:  ' , AS <= DS : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer86:  ' , BS <= AS : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer87:  ' , AS <= CS : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer88:  ' , DS <= AS : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer89:  ' , AS <= BS : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer90:  ' , DS >= AS : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer91:  ' , AS >= BS : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer92:  ' , AS >= CS : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer93:  ' , AS >= DS : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer94:  ' , BS >= AS : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer95:  ' , ABS ( AS ) : 1 , ' s/b 14' ) ;
  WRITELN ( 'Integer96:  ' , GS + HS : 1 , ' s/b 0' ) ;
  WRITELN ( 'Integer97:  ' , GS - MAXINT : 1 , ' s/b 0' ) ;
  WRITELN ( 'Integer98:  ' , GS + VNUM : 1 , ' s/b 0' ) ;

  (****************************)
  (* signed integer constants *)
  (****************************)

  WRITELN ( 'Integer99:  ' , 45 + ( - 30 ) : 1 , ' s/b 15' ) ;
  WRITELN ( 'Integer100:  ' , - 25 + 70 : 1 , ' s/b 45' ) ;
  WRITELN ( 'Integer101: ' , - 62 + 23 : 1 , ' s/b -39' ) ;
  WRITELN ( 'Integer102: ' , - 20 + ( - 15 ) : 1 , ' s/b -35' ) ;
  WRITELN ( 'Integer103: ' , 20 - ( - 14 ) : 1 , ' s/b 34' ) ;
  WRITELN ( 'Integer104: ' , - 34 - 14 : 1 , ' s/b -48' ) ;
  WRITELN ( 'Integer105: ' , - 56 - ( - 12 ) : 1 , ' s/b -44' ) ;
  WRITELN ( 'Integer106: ' , 5 * ( - 4 ) : 1 , ' s/b -20' ) ;
  WRITELN ( 'Integer107: ' , ( - 18 ) * 7 : 1 , ' s/b -126' ) ;
  WRITELN ( 'Integer108: ' , ( - 40 ) * ( - 13 ) : 1 , ' s/b 520' ) ;
  WRITELN ( 'Integer109: ' , 30 DIV ( - 5 ) : 1 , ' s/b -6' ) ;
  WRITELN ( 'Integer110: ' , ( - 50 ) DIV 2 : 1 , ' s/b -25' ) ;
  WRITELN ( 'Integer111: ' , ( - 20 ) DIV ( - 4 ) : 1 , ' s/b 5' ) ;
  WRITELN ( 'Integer112: ' , SUCC ( - 10 ) : 1 , ' s/b -9' ) ;
  WRITELN ( 'Integer113: ' , SUCC ( - 1 ) : 1 , ' s/b 0' ) ;
  WRITELN ( 'Integer114: ' , PRED ( - 1 ) : 1 , ' s/b -2' ) ;
  WRITELN ( 'Integer115: ' , SQR ( - 8 ) : 1 , ' s/b 64' ) ;
  WRITELN ( 'Integer116: ' , PRED ( - 54 ) : 1 , ' s/b -55' ) ;
  WRITELN ( 'Integer117: ' , ODD ( - 20 ) : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer118: ' , ODD ( - 15 ) : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer119: ' , - 5 = - 5 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer120: ' , - 5 = 5 : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer121: ' , - 21 <> - 40 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer122: ' , - 21 <> - 21 : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer123: ' , - 3 < 5 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer124: ' , - 32 < - 20 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer125: ' , 20 < - 20 : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer126: ' , - 15 < - 40 : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer127: ' , 70 > - 4 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer128: ' , - 23 > - 34 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer129: ' , - 5 > 5 : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer130: ' , - 60 > - 59 : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer131: ' , - 12 <= 4 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer132: ' , - 14 <= - 5 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer133: ' , - 7 <= - 7 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer134: ' , 5 <= - 5 : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer135: ' , - 10 <= - 20 : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer136: ' , 9 >= - 3 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer137: ' , - 4 >= - 10 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer138: ' , - 13 >= - 13 : 5 , ' s/b true' ) ;
  WRITELN ( 'Integer139: ' , - 6 >= 6 : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer140: ' , - 20 >= - 10 : 5 , ' s/b false' ) ;
  WRITELN ( 'Integer141: ' , ABS ( - 6 ) : 1 , ' s/b 6' ) ;
  WRITELN ( 'Integer142: ' , TSNCST : 1 , ' s/b -52' ) ;
  WRITELN ( 'Integer143: ' , - TSNCST : 1 , ' s/b 52' ) ;
  WRITELN ( 'Integer144: ' , TSNCST2 : 1 , ' s/b -768' ) ;
  WRITELN ( 'Integer145: ' , TSNCST3 : 1 , ' s/b 52' ) ;
  WRITELN ( 'Integer146: ' , MAXINT + MMAXINT : 1 , ' s/b 0' ) ;

  (*****************************************************************)
  (*                                                               *)
  (*                         Subranges                             *)
  (*                                                               *)
  (*****************************************************************)

  WRITELN ;
  WRITELN ( '******************* Subranges *******************' ) ;
  WRITELN ;

  (*******************************)
  (* subrange unsigned variables *)
  (*******************************)

  SRX := 43 ;
  SRY := 78 ;
  SRZ := SRY ;
  WRITELN ( 'Subrange1:   ' , SRX + SRY : 1 , ' s/b 121' ) ;
  WRITELN ( 'Subrange2:   ' , SRY - SRX : 1 , ' s/b 35' ) ;
  WRITELN ( 'Subrange3:   ' , SRX * SRY : 1 , ' s/b 3354' ) ;
  WRITELN ( 'Subrange4:   ' , SRY DIV SRX : 1 , ' s/b 1' ) ;
  WRITELN ( 'Subrange5:   ' , SRY MOD SRX : 1 , ' s/b 35' ) ;
  WRITELN ( 'Subrange6:   ' , SUCC ( SRX ) : 1 , ' s/b 44' ) ;
  WRITELN ( 'Subrange7:   ' , PRED ( SRX ) : 1 , ' s/b 42' ) ;
  WRITELN ( 'Subrange8:   ' , CHR ( SRY ) , ' s/b N' ) ;
  WRITELN ( 'Subrange9:   ' , ORD ( CHR ( SRX ) ) : 1 , ' s/b 43' ) ;
  WRITELN ( 'Subrange10:  ' , ODD ( SRX ) : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange11:  ' , ODD ( SRY ) : 5 , ' s/b false' ) ;
  WRITELN ( 'Subrange12:  ' , SRZ = SRY : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange13:  ' , SRX = SRY : 5 , ' s/b false' ) ;
  WRITELN ( 'Subrange14:  ' , SRX < SRY : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange15:  ' , SRY < SRX : 5 , ' s/b false' ) ;
  WRITELN ( 'Subrange16:  ' , SRY > SRX : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange17:  ' , SRX > SRY : 5 , ' s/b false' ) ;
  WRITELN ( 'Subrange18:  ' , SRX <> SRY : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange19:  ' , SRY <> SRZ : 5 , ' s/b false' ) ;
  WRITELN ( 'Subrange20:  ' , SRX <= SRY : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange21:  ' , SRZ <= SRY : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange22:  ' , SRY <= SRX : 5 , ' s/b false' ) ;
  WRITELN ( 'Subrange23:  ' , SRY >= SRX : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange24:  ' , SRY >= SRZ : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange25:  ' , SRX >= SRY : 5 , ' s/b false' ) ;

  (*****************************)
  (* signed subrange variables *)
  (*****************************)

  SRAS := - 14 ;
  SRBS := - 32 ;
  SRCS := - 14 ;
  SRDS := 20 ;
  SRES := - 15 ;
  WRITELN ( 'Subrange26:  ' , SRAS + SRDS : 1 , ' s/b 6' ) ;
  WRITELN ( 'Subrange27:  ' , SRDS + SRAS : 1 , ' s/b 6' ) ;
  WRITELN ( 'Subrange28:  ' , SRBS + SRDS : 1 , ' s/b -12' ) ;
  WRITELN ( 'Subrange29:  ' , SRAS + SRBS : 1 , ' s/b -46' ) ;
  WRITELN ( 'Subrange30:  ' , SRDS - SRAS : 1 , ' s/b 34' ) ;
  WRITELN ( 'Subrange31:  ' , SRBS - SRDS : 1 , ' s/b -52' ) ;
  WRITELN ( 'Subrange32:  ' , SRBS - SRAS : 1 , ' s/b -18' ) ;
  WRITELN ( 'Subrange33:  ' , SRDS * SRAS : 1 , ' s/b -280' ) ;
  WRITELN ( 'Subrange34:  ' , SRAS * SRDS : 1 , ' s/b -280' ) ;
  WRITELN ( 'Subrange35:  ' , SRAS * SRBS : 1 , ' s/b 448' ) ;
  WRITELN ( 'Subrange36:  ' , SRDS DIV SRAS : 1 , ' s/b -1' ) ;
  WRITELN ( 'Subrange37:  ' , SRBS DIV SRDS : 1 , ' s/b -1' ) ;
  WRITELN ( 'Subrange38:  ' , SRBS DIV SRAS : 1 , ' s/b 2' ) ;
  WRITELN ( 'Subrange39:  ' , SUCC ( SRAS ) : 1 , ' s/b -13' ) ;
  WRITELN ( 'Subrange40:  ' , PRED ( SRBS ) : 1 , ' s/b -33' ) ;
  WRITELN ( 'Subrange41:  ' , ODD ( SRAS ) : 5 , ' s/b false' ) ;
  WRITELN ( 'Subrange42:  ' , ODD ( SRES ) : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange43:  ' , SRAS = SRCS : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange44:  ' , SRAS = SRBS : 5 , ' s/b false' ) ;
  WRITELN ( 'Subrange45:  ' , SRAS <> SRBS : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange46:  ' , SRAS <> SRCS : 5 , ' s/b false' ) ;
  WRITELN ( 'Subrange47:  ' , SRAS < SRDS : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange48:  ' , SRBS < SRAS : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange49:  ' , SRDS < SRAS : 5 , ' s/b false' ) ;
  WRITELN ( 'Subrange50:  ' , SRAS < SRBS : 5 , ' s/b false' ) ;
  WRITELN ( 'Subrange51:  ' , SRDS > SRAS : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange52:  ' , SRAS > SRBS : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange53:  ' , SRAS > SRDS : 5 , ' s/b false' ) ;
  WRITELN ( 'Subrange54:  ' , SRBS > SRAS : 5 , ' s/b false' ) ;
  WRITELN ( 'Subrange55:  ' , SRAS <= SRDS : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange56:  ' , SRBS <= SRAS : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange57:  ' , SRAS <= SRCS : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange58:  ' , SRDS <= SRAS : 5 , ' s/b false' ) ;
  WRITELN ( 'Subrange59:  ' , SRAS <= SRBS : 5 , ' s/b false' ) ;
  WRITELN ( 'Subrange60:  ' , SRDS >= SRAS : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange61:  ' , SRAS >= SRBS : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange62:  ' , SRAS >= SRCS : 5 , ' s/b true' ) ;
  WRITELN ( 'Subrange63:  ' , SRAS >= SRDS : 5 , ' s/b false' ) ;
  WRITELN ( 'Subrange64:  ' , SRBS >= SRAS : 5 , ' s/b false' ) ;
  WRITELN ( 'Subrange65:  ' , ABS ( SRAS ) : 1 , ' s/b 14' ) ;

  (*****************************************************************)
  (*                                                               *)
  (*                      Characters                               *)
  (*                                                               *)
  (*****************************************************************)

  WRITELN ;
  WRITELN ( '******************* Characters*******************' ) ;
  WRITELN ;

  (***********************)
  (* character variables *)
  (***********************)

  CA := 'g' ;
  CB := 'g' ;
  CC := 'u' ;
  WRITELN ( 'Character1:   ' , CA , ' ' , CB , ' ' , CC , ' s/b g g u'
            ) ;
  WRITELN ( 'Character2:   ' , SUCC ( CA ) , ' s/b h' ) ;
  WRITELN ( 'Character3:   ' , PRED ( CB ) , ' s/b f' ) ;
  WRITELN ( 'Character4:   ' , ORD ( CA ) : 1 , ' s/b 103' ) ;
  WRITELN ( 'Character5:   ' , CHR ( ORD ( CC ) ) , ' s/b u' ) ;
  WRITELN ( 'Character6:   ' , CA = CB : 5 , ' s/b true' ) ;
  WRITELN ( 'Character7:   ' , CA = CC : 5 , ' s/b false' ) ;
  WRITELN ( 'Character8:   ' , CA < CC : 5 , ' s/b true' ) ;
  WRITELN ( 'Character9:   ' , CC < CA : 5 , ' s/b false' ) ;
  WRITELN ( 'Character10:  ' , CC > CA : 5 , ' s/b true' ) ;
  WRITELN ( 'Character11:  ' , CA > CC : 5 , ' s/b false' ) ;
  WRITELN ( 'Character12:  ' , CA <> CC : 5 , ' s/b true' ) ;
  WRITELN ( 'Character13:  ' , CA <> CB : 5 , ' s/b false' ) ;
  WRITELN ( 'Character14:  ' , CA <= CC : 5 , ' s/b true' ) ;
  WRITELN ( 'Character15:  ' , CA <= CB : 5 , ' s/b true' ) ;
  WRITELN ( 'Character16:  ' , CC <= CA : 5 , ' s/b false' ) ;
  WRITELN ( 'Character17:  ' , CC >= CB : 5 , ' s/b true' ) ;
  WRITELN ( 'Character18:  ' , CB >= CA : 5 , ' s/b true' ) ;
  WRITELN ( 'Character19:  ' , CB >= CC : 5 , ' s/b false' ) ;
  SA := 'porker    ' ;
  SB := 'porker    ' ;
  SC := 'parker    ' ;
  WRITELN ( 'Character20:  ' , SA , SB , SC ,
            ' s/b porker    porker    parker' ) ;
  WRITELN ( 'Character21:  ' , SA = SB : 5 , ' s/b true' ) ;
  WRITELN ( 'Character22:  ' , SA = SC : 5 , ' s/b false' ) ;
  WRITELN ( 'Character23:  ' , SC < SA : 5 , ' s/b true' ) ;
  WRITELN ( 'Character24:  ' , SA < SC : 5 , ' s/b false' ) ;
  WRITELN ( 'Character25:  ' , SA > SC : 5 , ' s/b true' ) ;
  WRITELN ( 'Character26:  ' , SC > SA : 5 , ' s/b false' ) ;
  WRITELN ( 'Character27:  ' , SA <> SC : 5 , ' s/b true' ) ;
  WRITELN ( 'Character28:  ' , SA <> SB : 5 , ' s/b false' ) ;
  WRITELN ( 'Character29:  ' , SC <= SA : 5 , ' s/b true' ) ;
  WRITELN ( 'Character30:  ' , SA <= SB : 5 , ' s/b true' ) ;
  WRITELN ( 'Character40:  ' , SA <= SC : 5 , ' s/b false' ) ;
  WRITELN ( 'Character41:  ' , SA >= SC : 5 , ' s/b true' ) ;
  WRITELN ( 'Character42:  ' , SA >= SB : 5 , ' s/b true' ) ;
  WRITELN ( 'Character43:  ' , SC >= SA : 5 , ' s/b false' ) ;
  WRITE ( 'Character44:  ' ) ;
  for CA := 'a' to 'z' do
    WRITE ( CA ) ;
  WRITELN ( ' s/b abcdefghijklmnopqrstuvwxyz' ) ;
  WRITE ( 'Character45:  ' ) ;
  for CA := 'z' DOWNTO 'a' do
    WRITE ( CA ) ;
  WRITELN ( ' s/b zyxwvutsrqponmlkjihgfedcba' ) ;
  WRITE ( 'Character46:  ' ) ;
  X := 0 ;
  for CA := 'a' to 'z' do
    begin
      CAR [ CA ] := X ;
      X := X + 1
    end (* for *) ;
  for CA := 'z' DOWNTO 'a' do
    WRITE ( CAR [ CA ] : 1 , ' ' ) ;
  WRITELN ;
  WRITELN ( 'Character46: s/b 25 24 23 22 21 20 19 18 17 16 15' ,
            ' 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0' ) ;
  R . RC := 'n' ;
  WRITELN ( 'Character47: ' , R . RC , ' s/b n' ) ;
  R . RS := 'junky01234' ;
  WRITELN ( 'Character48: ' , R . RS , ' s/b junky01234' ) ;
  for I := 1 to 10 do
    SAR [ I ] := '0123456789' ;
  SAR [ 1 ] := 'trash     ' ;
  SAR [ 2 ] := 'finnork   ' ;
  SAR [ 10 ] := 'crapola   ' ;
  WRITELN ( 'Character49:  ' ) ;
  for I := 10 DOWNTO 1 do
    WRITELN ( SAR [ I ] ) ;
  WRITELN ( 'Character49: s/b' ) ;
  WRITELN ( 'crapola' ) ;
  WRITELN ( '0123456789' ) ;
  WRITELN ( '0123456789' ) ;
  WRITELN ( '0123456789' ) ;
  WRITELN ( '0123456789' ) ;
  WRITELN ( '0123456789' ) ;
  WRITELN ( '0123456789' ) ;
  WRITELN ( '0123456789' ) ;
  WRITELN ( 'finnork' ) ;
  WRITELN ( 'trash' ) ;
  WRITELN ( 'Character50:  ' ) ;
  for CA := '0' to '9' do
    begin
      case CA of
        '5' : WRITE ( 'five ' ) ;
        '3' : WRITE ( 'three ' ) ;
        '6' : WRITE ( 'six ' ) ;
        '8' : WRITE ( 'eight ' ) ;
        '0' : WRITE ( 'zero ' ) ;
        '9' : WRITE ( 'nine ' ) ;
        '7' : WRITE ( 'seven ' ) ;
        '4' : WRITE ( 'four ' ) ;
        '1' : WRITE ( 'one ' ) ;
        '2' : WRITE ( 'two ' ) ;
      end (* case *)
    end (* for *) ;
  WRITELN ;
  WRITELN ( ' s/b zero one two three four five six ' ,
            'seven eight nine' ) ;

  (***********************)
  (* character constants *)
  (***********************)

  WRITELN ( 'Character51:  ' , 'a' , ' s/b a' ) ;
  WRITELN ( 'Character52:  ' , SUCC ( 'a' ) , ' s/b b' ) ;
  WRITELN ( 'Character53:  ' , PRED ( 'z' ) , ' s/b y' ) ;
  WRITELN ( 'Character54:  ' , ORD ( 'c' ) : 1 , ' s/b 99' ) ;
  WRITELN ( 'Character55:  ' , CHR ( ORD ( 'g' ) ) , ' s/b g' ) ;
  WRITELN ( 'Character56:  ' , 'q' = 'q' : 5 , ' s/b true' ) ;
  WRITELN ( 'Character57:  ' , 'r' = 'q' : 5 , ' s/b false' ) ;
  WRITELN ( 'Character58:  ' , 'b' < 't' : 5 , ' s/b true' ) ;
  WRITELN ( 'Character59:  ' , 'g' < 'c' : 5 , ' s/b false' ) ;
  WRITELN ( 'Character50:  ' , 'f' > 'e' : 5 , ' s/b true' ) ;
  WRITELN ( 'Character61:  ' , 'f' > 'g' : 5 , ' s/b false' ) ;
  WRITELN ( 'Character62:  ' , 'h' <> 'l' : 5 , ' s/b true' ) ;
  WRITELN ( 'Character63:  ' , 'i' <> 'i' : 5 , ' s/b false' ) ;
  WRITELN ( 'Character64:  ' , 'v' <= 'y' : 5 , ' s/b true' ) ;
  WRITELN ( 'Character65:  ' , 'y' <= 'y' : 5 , ' s/b true' ) ;
  WRITELN ( 'Character66:  ' , 'z' <= 'y' : 5 , ' s/b false' ) ;
  WRITELN ( 'Character67:  ' , 'l' >= 'b' : 5 , ' s/b true' ) ;
  WRITELN ( 'Character68:  ' , 'l' >= 'l' : 5 , ' s/b true' ) ;
  WRITELN ( 'Character69:  ' , 'l' >= 'm' : 5 , ' s/b false' ) ;
  WRITELN ( 'Character70:  ' , 'finnork' = 'finnork' : 5 , ' s/b true'
            ) ;
  WRITELN ( 'Character71:  ' , 'finoork' = 'finnork' : 5 , ' s/b false'
            ) ;
  WRITELN ( 'Character72:  ' , 'oliab' < 'olibb' : 5 , ' s/b true' ) ;
  WRITELN ( 'Character73:  ' , 'olibb' < 'oliab' : 5 , ' s/b false' ) ;
  WRITELN ( 'Character74:  ' , 'olibb' > 'oliab' : 5 , ' s/b true' ) ;
  WRITELN ( 'Character75:  ' , 'oliab' > 'olibb' : 5 , ' s/b false' ) ;
  WRITELN ( 'Character76:  ' , 'fark ' <> 'farks' : 5 , ' s/b true' ) ;
  WRITELN ( 'Character77:  ' , 'farks' <> 'farks' : 5 , ' s/b false' )
            ;
  WRITELN ( 'Character78:  ' , 'farka' <= 'farkz' : 5 , ' s/b true' ) ;
  WRITELN ( 'Character79:  ' , 'farks' <= 'farks' : 5 , ' s/b true' ) ;
  WRITELN ( 'Character80:  ' , 'farkz' <= 'farks' : 5 , ' s/b false' )
            ;
  WRITELN ( 'Character81:  ' , 'topnat' >= 'topcat' : 5 , ' s/b true' )
            ;
  WRITELN ( 'Character82:  ' , 'topcat' >= 'topcat' : 5 , ' s/b true' )
            ;
  WRITELN ( 'Character83:  ' , 'topcat' >= 'topzat' : 5 , ' s/b false'
            ) ;
  WRITELN ( 'Character84:  ' , SCST , ' s/b this is a string' ) ;
  WRITELN ( 'Character85:  ' , CCST , ' s/b v' ) ;
  WRITELN ( 'Character86:  ' ) ;
  for I := 15 DOWNTO 1 do
    WRITELN ( 'hello, world' : I ) ;
  WRITELN ( 'Character86:  s/b:' ) ;
  WRITELN ( '   hello, world' ) ;
  WRITELN ( '  hello, world' ) ;
  WRITELN ( ' hello, world ' ) ;
  WRITELN ( 'hello, world' ) ;
  WRITELN ( 'hello, worl' ) ;
  WRITELN ( 'hello, wor' ) ;
  WRITELN ( 'hello, wo' ) ;
  WRITELN ( 'hello, w' ) ;
  WRITELN ( 'hello, ' ) ;
  WRITELN ( 'hello,' ) ;
  WRITELN ( 'hello' ) ;
  WRITELN ( 'hell' ) ;
  WRITELN ( 'hel' ) ;
  WRITELN ( 'he' ) ;
  WRITELN ( 'h' ) ;

  (*****************************************************************)
  (*                                                               *)
  (*                         Booleans                              *)
  (*                                                               *)
  (*****************************************************************)

  WRITELN ;
  WRITELN ( '******************* Booleans *******************' ) ;
  WRITELN ;

  (*********************)
  (* boolean variables *)
  (*********************)

  BA := TRUE ;
  BB := FALSE ;
  BC := TRUE ;
  WRITELN ( 'Boolean1:   ' , BA : 5 , ' ' , BB : 5 , ' s/b true false'
            ) ;
  WRITELN ( 'Boolean2:   ' , SUCC ( BB ) : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean3:   ' , PRED ( BA ) : 5 , ' s/b false' ) ;
  WRITELN ( 'Boolean4:   ' , ORD ( BB ) : 1 , ' s/b 0' ) ;
  WRITELN ( 'Boolean5:   ' , ORD ( BA ) : 1 , ' s/b 1' ) ;
  WRITELN ( 'Boolean6:   ' , BA = BC : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean7:   ' , BB = BB : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean8:   ' , BA = BB : 5 , ' s/b false' ) ;
  WRITELN ( 'Boolean9:   ' , BB < BA : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean10:  ' , BA < BB : 5 , ' s/b false' ) ;
  WRITELN ( 'Boolean11:  ' , BA > BB : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean12:  ' , BB > BA : 5 , ' s/b false' ) ;
  WRITELN ( 'Boolean13:  ' , BA <> BB : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean14:  ' , BA <> BC : 5 , ' s/b false' ) ;
  WRITELN ( 'Boolean15:  ' , BB <= BA : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean16:  ' , BA <= BC : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean17:  ' , BA <= BB : 5 , ' s/b false' ) ;
  WRITELN ( 'Boolean18:  ' , BA >= BB : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean19:  ' , BB >= BB : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean20:  ' , BB >= BA : 5 , ' s/b false' ) ;
  WRITE ( 'Boolean21:  ' ) ;
  for BA := FALSE to TRUE do
    WRITE ( BA : 5 , ' ' ) ;
  WRITELN ( 's/b false true' ) ;
  WRITE ( 'Boolean22:  ' ) ;
  for BB := TRUE DOWNTO FALSE do
    WRITE ( BB : 5 , ' ' ) ;
  WRITELN ( 's/b true false' ) ;
  WRITE ( 'Boolean23:  ' ) ;
  BA := 1 > 0 ;
  WRITELN ( BA : 5 , ' s/b true' ) ;
  WRITE ( 'Boolean24:  ' ) ;
  BA := 1 < 0 ;
  WRITELN ( BA : 5 , ' s/b false' ) ;

  (*********************)
  (* boolean constants *)
  (*********************)

  WRITELN ( 'Boolean25:  ' , TRUE : 5 , ' ' , FALSE : 5 ,
            ' s/b true false' ) ;
  WRITELN ( 'Boolean26:  ' , SUCC ( FALSE ) : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean27:  ' , PRED ( TRUE ) : 5 , ' s/b false' ) ;
  WRITELN ( 'Boolean28:  ' , ORD ( FALSE ) : 1 , ' s/b 0' ) ;
  WRITELN ( 'Boolean29:  ' , ORD ( TRUE ) : 1 , ' s/b 1' ) ;
  WRITELN ( 'Boolean30:  ' , TRUE = TRUE : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean31:  ' , FALSE = FALSE : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean32:  ' , TRUE = FALSE : 5 , ' s/b false' ) ;
  WRITELN ( 'Boolean33:  ' , FALSE < TRUE : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean34:  ' , TRUE < FALSE : 5 , ' s/b false' ) ;
  WRITELN ( 'Boolean35:  ' , TRUE > FALSE : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean36:  ' , FALSE > TRUE : 5 , ' s/b false' ) ;
  WRITELN ( 'Boolean37:  ' , TRUE <> FALSE : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean38:  ' , TRUE <> TRUE : 5 , ' s/b false' ) ;
  WRITELN ( 'Boolean39:  ' , FALSE <= TRUE : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean40:  ' , TRUE <= TRUE : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean41:  ' , TRUE <= FALSE : 5 , ' s/b false' ) ;
  WRITELN ( 'Boolean42:  ' , TRUE >= FALSE : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean43:  ' , FALSE >= FALSE : 5 , ' s/b true' ) ;
  WRITELN ( 'Boolean44:  ' , FALSE >= TRUE : 5 , ' s/b false' ) ;
  WRITELN ( 'Boolean45:' ) ;
  for I := 10 DOWNTO 1 do
    WRITELN ( FALSE : I ) ;
  WRITELN ( 'Boolean45: s/b:' ) ;
  WRITELN ( '     false' ) ;
  WRITELN ( '    false' ) ;
  WRITELN ( '   false' ) ;
  WRITELN ( '  false' ) ;
  WRITELN ( ' false' ) ;
  WRITELN ( 'false' ) ;
  WRITELN ( 'fals' ) ;
  WRITELN ( 'fal' ) ;
  WRITELN ( 'fa' ) ;
  WRITELN ( 'f' ) ;
  WRITELN ( 'Boolean46:' ) ;
  for I := 10 DOWNTO 1 do
    WRITELN ( TRUE : I ) ;
  WRITELN ( 'Boolean46: s/b:' ) ;
  WRITELN ( '      true' ) ;
  WRITELN ( '     true' ) ;
  WRITELN ( '    true' ) ;
  WRITELN ( '   true' ) ;
  WRITELN ( '  true' ) ;
  WRITELN ( ' true' ) ;
  WRITELN ( 'true' ) ;
  WRITELN ( 'tru' ) ;
  WRITELN ( 'tr' ) ;
  WRITELN ( 't' ) ;

  (*****************************************************************)
  (*                                                               *)
  (*                         Scalar variables                      *)
  (*                                                               *)
  (*****************************************************************)

  WRITELN ;
  WRITELN ( '******************* Scalar *******************' ) ;
  WRITELN ;

  (********************)
  (* scalar variables *)
  (********************)

  SVA := WED ;
  SVB := MON ;
  SVC := WED ;
  WRITELN ( 'Scalar1:   ' , SUCC ( SVB ) = TUE : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar2:   ' , PRED ( SVA ) = TUE : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar3:   ' , ORD ( SVB ) : 1 , ' s/b 0' ) ;
  WRITELN ( 'Scalar4:   ' , ORD ( SVA ) : 1 , ' s/b 2' ) ;
  WRITELN ( 'Scalar5:   ' , SVA = SVC : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar6:   ' , SVB = SVB : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar7:   ' , SVA = SVB : 5 , ' s/b false' ) ;
  WRITELN ( 'Scalar8:   ' , SVB < SVA : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar9:   ' , SVA < SVB : 5 , ' s/b false' ) ;
  WRITELN ( 'Scalar10:  ' , SVA > SVB : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar11:  ' , SVB > SVA : 5 , ' s/b false' ) ;
  WRITELN ( 'Scalar12:  ' , SVA <> SVB : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar13:  ' , SVA <> SVC : 5 , ' s/b false' ) ;
  WRITELN ( 'Scalar14:  ' , SVB <= SVA : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar15:  ' , SVA <= SVC : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar16:  ' , SVA <= SVB : 5 , ' s/b false' ) ;
  WRITELN ( 'Scalar17:  ' , SVA >= SVB : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar18:  ' , SVB >= SVB : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar19:  ' , SVB >= SVA : 5 , ' s/b false' ) ;
  WRITE ( 'Scalar20:  ' ) ;
  for SVA := MON to SUN do
    WRITE ( ORD ( SVA ) : 1 , ' ' ) ;
  WRITELN ( 's/b 0 1 2 3 4 5 6' ) ;
  WRITE ( 'Scalar21:  ' ) ;
  for SVB := SUN DOWNTO MON do
    WRITE ( ORD ( SVB ) : 1 , ' ' ) ;
  WRITELN ( 's/b 6 5 4 3 2 1 0' ) ;

  (********************)
  (* scalar constants *)
  (********************)

  WRITELN ( 'Scalar1:   ' , SUCC ( MON ) = TUE : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar2:   ' , PRED ( FRI ) = THUR : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar3:   ' , ORD ( WED ) : 1 , ' s/b 2' ) ;
  WRITELN ( 'Scalar4:   ' , ORD ( SUN ) : 1 , ' s/b 6' ) ;
  WRITELN ( 'Scalar5:   ' , THUR = THUR : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar6:   ' , FRI = FRI : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar7:   ' , TUE = WED : 5 , ' s/b false' ) ;
  WRITELN ( 'Scalar8:   ' , MON < WED : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar9:   ' , FRI < FRI : 5 , ' s/b false' ) ;
  WRITELN ( 'Scalar10:  ' , SUN > SAT : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar11:  ' , FRI > SUN : 5 , ' s/b false' ) ;
  WRITELN ( 'Scalar12:  ' , THUR <> TUE : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar13:  ' , WED <> WED : 5 , ' s/b false' ) ;
  WRITELN ( 'Scalar14:  ' , MON <= FRI : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar15:  ' , FRI <= FRI : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar16:  ' , SAT <= FRI : 5 , ' s/b false' ) ;
  WRITELN ( 'Scalar17:  ' , FRI >= TUE : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar18:  ' , TUE >= TUE : 5 , ' s/b true' ) ;
  WRITELN ( 'Scalar19:  ' , TUE >= SAT : 5 , ' s/b false' ) ;

  (*****************************************************************)
  (*                                                               *)
  (*                         Reals                                 *)
  (*                                                               *)
  (*****************************************************************)

  WRITELN ;
  WRITELN ( '******************* Reals ******************************'
            ) ;
  WRITELN ;

  (****************************************)
  (* formats, input (compiler) and output *)
  (****************************************)

  WRITELN ( 'Real1:   ' , 1.554 : 15 , ' s/b  1.554000e+00' ) ;
  WRITELN ( 'Real2:   ' , 0.00334 : 15 , ' s/b  3.340000e-03' ) ;
  WRITELN ( 'Real3:   ' , 0.00334E-21 : 15 , ' s/b  3.34000e-24' ) ;
  WRITELN ( 'Real4:   ' , 4E-45 : 15 , ' s/b  4.000000e-45' ) ;
  WRITELN ( 'Real5:   ' , - 5.565 : 15 , ' s/b -5.565000e+03' ) ;
  WRITELN ( 'Real6:   ' , - 0.00944 : 15 , ' s/b -9.440000e-03' ) ;
  WRITELN ( 'Real7:   ' , - 0.006364E+32 : 15 , ' s/b -6.364000e+29' )
            ;
  WRITELN ( 'Real8:   ' , - 2E-14 : 15 , ' s/b -2.000000e-14' ) ;
  WRITELN ( 'Real9:' ) ;
  WRITELN ( '         11111111112222222222333333333344444444445' ) ;
  WRITELN ( '12345678901234567890123456789012345678901234567890' ) ;
  for I := 1 to 20 do
    WRITELN ( 1.23456789012345678901234567890 : I ) ;
  WRITELN ( 's/b (note precision dropoff at right):' ) ;
  WRITELN ( ' 1.2e+000' ) ;
  WRITELN ( ' 1.2e+000' ) ;
  WRITELN ( ' 1.2e+000' ) ;
  WRITELN ( ' 1.2e+000' ) ;
  WRITELN ( ' 1.2e+000' ) ;
  WRITELN ( ' 1.2e+000' ) ;
  WRITELN ( ' 1.2e+000' ) ;
  WRITELN ( ' 1.2e+000' ) ;
  WRITELN ( ' 1.2e+000' ) ;
  WRITELN ( ' 1.23e+000' ) ;
  WRITELN ( ' 1.234e+000' ) ;
  WRITELN ( ' 1.2345e+000' ) ;
  WRITELN ( ' 1.23456e+000' ) ;
  WRITELN ( ' 1.234567e+000' ) ;
  WRITELN ( ' 1.2345678e+000' ) ;
  WRITELN ( ' 1.23456789e+000' ) ;
  WRITELN ( ' 1.234567890e+000' ) ;
  WRITELN ( ' 1.2345678901e+000' ) ;
  WRITELN ( ' 1.23456789012e+000' ) ;
  WRITELN ( ' 1.234567890123e+000' ) ;
  WRITELN ( 'Real10:' ) ;
  WRITELN ( '         11111111112222222222333333333344444444445' ) ;
  WRITELN ( '12345678901234567890123456789012345678901234567890' ) ;
  for I := 1 to 20 do
    WRITELN ( I + 0.23456789012345678901234567890 : 1 : I ) ;
  WRITELN ( 's/b (note precision dropoff at right):' ) ;
  WRITELN ( '1.2' ) ;
  WRITELN ( '2.23' ) ;
  WRITELN ( '3.234' ) ;
  WRITELN ( '4.2345' ) ;
  WRITELN ( '5.23456' ) ;
  WRITELN ( '6.234567' ) ;
  WRITELN ( '7.2345678' ) ;
  WRITELN ( '8.23456789' ) ;
  WRITELN ( '9.234567890' ) ;
  WRITELN ( '10.2345678901' ) ;
  WRITELN ( '11.23456789012' ) ;
  WRITELN ( '12.234567890123' ) ;
  WRITELN ( '13.2345678901234' ) ;
  WRITELN ( '14.23456789012345' ) ;
  WRITELN ( '15.234567890123456' ) ;
  WRITELN ( '16.2345678901234567' ) ;
  WRITELN ( '17.23456789012345678' ) ;
  WRITELN ( '18.234567890123456789' ) ;
  WRITELN ( '19.2345678901234567890' ) ;
  WRITELN ( '20.23456789012345678901' ) ;

  (**********************)
  (* unsigned variables *)
  (**********************)

  RA := 435.23 ;
  RB := 983.67 ;
  RC := RB ;
  RD := 0.3443 ;
  WRITELN ( 'Real11:  ' , RA + RB : 15 , ' s/b  1.418900e+03' ) ;
  WRITELN ( 'Rea112:  ' , RB - RA : 15 , ' s/b  5.484399e+02' ) ;
  WRITELN ( 'Real13:  ' , RA * RB : 15 , ' s/b  4.281227e+05' ) ;
  WRITELN ( 'Real14:  ' , RB / RA : 15 , ' s/b  2.260115e+00' ) ;
  WRITELN ( 'Real15:  ' , RC = RB : 5 , ' s/b true' ) ;
  WRITELN ( 'Real16:  ' , RA = RB : 5 , ' s/b false' ) ;
  WRITELN ( 'Real17:  ' , RA < RB : 5 , ' s/b true' ) ;
  WRITELN ( 'Real18:  ' , RB < RA : 5 , ' s/b false' ) ;
  WRITELN ( 'Real19:  ' , RB > RA : 5 , ' s/b true' ) ;
  WRITELN ( 'Real20:  ' , RA > RB : 5 , ' s/b false' ) ;
  WRITELN ( 'Real21:  ' , RA <> RB : 5 , ' s/b true' ) ;
  WRITELN ( 'Real22:  ' , RB <> RC : 5 , ' s/b false' ) ;
  WRITELN ( 'Real23:  ' , RA <= RB : 5 , ' s/b true' ) ;
  WRITELN ( 'Real24:  ' , RC <= RB : 5 , ' s/b true' ) ;
  WRITELN ( 'Real25:  ' , RB <= RA : 5 , ' s/b false' ) ;
  WRITELN ( 'Real26:  ' , RB >= RA : 5 , ' s/b true' ) ;
  WRITELN ( 'Real27:  ' , RB >= RC : 5 , ' s/b true' ) ;
  WRITELN ( 'Real28:  ' , RA >= RB : 5 , ' s/b false' ) ;
  WRITELN ( 'Real29:  ' , ABS ( RA ) : 15 , ' s/b  4.35230e+02' ) ;
  WRITELN ( 'Real30:  ' , SQR ( RA ) : 15 , ' s/b  1.89425e+05' ) ;
  WRITELN ( 'Real31:  ' , SQRT ( RB ) : 15 , ' s/b  3.13635e+01' ) ;
  WRITELN ( 'Real32:  ' , SIN ( RB ) : 15 , ' s/b -3.44290e-01' ) ;
  WRITELN ( 'Real33:  ' , ARCTAN ( RA ) : 15 , ' s/b  1.56850e+00' ) ;
  WRITELN ( 'Real34:  ' , EXP ( RD ) : 15 , ' s/b  1.41100e+00' ) ;
  WRITELN ( 'Real35:  ' , LN ( RA ) : 15 , ' s/b  6.07587e+00' ) ;
  WRITELN ( 'Real36:  ' , TRUNC ( RA ) : 1 , ' s/b 435' ) ;
  WRITELN ( 'Real37:  ' , ROUND ( RB ) : 1 , ' s/b 984' ) ;
  WRITELN ( 'Real38:  ' , ROUND ( RA ) : 1 , ' s/b 435' ) ;

  (**********************)
  (* unsigned constants *)
  (**********************)

  WRITELN ( 'Real39:  ' , 344.939 + 933.113 : 15 , ' s/b  1.278052e+03'
            ) ;
  WRITELN ( 'Real40:  ' , 883.885 - 644.939 : 15 , ' s/b  2.389460e+02'
            ) ;
  WRITELN ( 'Real41:  ' , 754.74 * 138.75 : 15 , ' s/b  1.047202e+05' )
            ;
  WRITELN ( 'Real42:  ' , 634.3 / 87373.99 : 15 , ' s/b  7.259598e-03'
            ) ;
  WRITELN ( 'Real43:  ' , 77.44 = 77.44 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real44:  ' , 733.9 = 959.2 : 5 , ' s/b false' ) ;
  WRITELN ( 'Real45:  ' , 883.22 < 8383.33 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real46:  ' , 475.322 < 234.93 : 5 , ' s/b false' ) ;
  WRITELN ( 'Real47:  ' , 7374.3 > 6442.34 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real48:  ' , 985.562 > 1001.95 : 5 , ' s/b false' ) ;
  WRITELN ( 'Real49:  ' , 030.11 <> 0938.44 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real50:  ' , 1.233 <> 1.233 : 5 , ' s/b false' ) ;
  WRITELN ( 'Real51:  ' , 8484.002 <= 9344.003 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real52:  ' , 9.11 <= 9.11 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real53:  ' , 93.323 <= 90.323 : 5 , ' s/b false' ) ;
  WRITELN ( 'Real54:  ' , 6543.44 >= 5883.33 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real55:  ' , 3247.03 >= 3247.03 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real56:  ' , 28343.22 >= 30044.45 : 5 , ' s/b false' ) ;
  WRITELN ( 'Real57:  ' , ABS ( 34.93 ) : 15 , ' s/b  3.493000e+01' ) ;
  WRITELN ( 'Real58:  ' , SQR ( 2.34 ) : 15 , ' s/b  5.475600e+00' ) ;
  WRITELN ( 'Real59:  ' , SQRT ( 9454.32 ) : 15 , ' s/b  9.723333e+01'
            ) ;
  WRITELN ( 'Real60:  ' , SIN ( 34.22 ) : 15 , ' s/b  3.311461e-01' ) ;
  WRITELN ( 'Real61:  ' , ARCTAN ( 343.2 ) : 15 , ' s/b  1.567883e+00'
            ) ;
  WRITELN ( 'Real62:  ' , EXP ( 0.332 ) : 15 , ' s/b  1.393753e+00' ) ;
  WRITELN ( 'Real63:  ' , LN ( 83.22 ) : 15 , ' s/b  4.421488e+00' ) ;
  WRITELN ( 'Real64:  ' , TRUNC ( 24.344 ) : 1 , ' s/b 24' ) ;
  WRITELN ( 'Real65:  ' , ROUND ( 74.56 ) : 1 , ' s/b 75' ) ;
  WRITELN ( 'Real66:  ' , ROUND ( 83.24 ) : 1 , ' s/b 83' ) ;
  WRITELN ( 'Real67:  ' , RCNST : 15 , ' s/b  4.333000e+01' ) ;

  (********************)
  (* signed variables *)
  (********************)

  RA := - 734.2 ;
  RB := - 7634.52 ;
  RC := RA ;
  RD := 1034.54 ;
  RE := - 0.38483 ;
  WRITELN ( 'Real68:  ' , RA + RD : 15 , ' s/b  3.003400e+02' ) ;
  WRITELN ( 'Real69:  ' , RD + RA : 15 , ' s/b  3.003400e+02' ) ;
  WRITELN ( 'Real70:  ' , RB + RD : 15 , ' s/b -6.599980e+03' ) ;
  WRITELN ( 'Real71:  ' , RA + RB : 15 , ' s/b -8.368720e+03' ) ;
  WRITELN ( 'Real72:  ' , RD - RA : 15 , ' s/b  1.768740e+03' ) ;
  WRITELN ( 'Real73:  ' , RB - RD : 15 , ' s/b -8.669061e+03' ) ;
  WRITELN ( 'Real74:  ' , RB - RA : 15 , ' s/b -6.900320e+03' ) ;
  WRITELN ( 'Real75:  ' , RD * RA : 15 , ' s/b -7.595593e+05' ) ;
  WRITELN ( 'Real76:  ' , RA * RD : 15 , ' s/b -7.595593e+05' ) ;
  WRITELN ( 'Real77:  ' , RA * RB : 15 , ' s/b  5.605265e+06' ) ;
  WRITELN ( 'Real78:  ' , RD / RA : 15 , ' s/b -1.409071e+00' ) ;
  WRITELN ( 'Real79:  ' , RB / RD : 15 , ' s/b -7.379627e+00' ) ;
  WRITELN ( 'Real80:  ' , RB / RA : 15 , ' s/b  1.039842e+01' ) ;
  WRITELN ( 'Real81:  ' , RA = RC : 5 , ' s/b true' ) ;
  WRITELN ( 'Real82:  ' , RA = RB : 5 , ' s/b false' ) ;
  WRITELN ( 'Real83:  ' , RA <> RB : 5 , ' s/b true' ) ;
  WRITELN ( 'Real84:  ' , RA <> RC : 5 , ' s/b false' ) ;
  WRITELN ( 'Real85:  ' , RA < RD : 5 , ' s/b true' ) ;
  WRITELN ( 'Real86:  ' , RB < RA : 5 , ' s/b true' ) ;
  WRITELN ( 'Real87:  ' , RD < RA : 5 , ' s/b false' ) ;
  WRITELN ( 'Real88:  ' , RA < RB : 5 , ' s/b false' ) ;
  WRITELN ( 'Real89:  ' , RD > RA : 5 , ' s/b true' ) ;
  WRITELN ( 'Real90:  ' , RA > RB : 5 , ' s/b true' ) ;
  WRITELN ( 'Real91:  ' , RA > RD : 5 , ' s/b false' ) ;
  WRITELN ( 'Real92:  ' , RB > RA : 5 , ' s/b false' ) ;
  WRITELN ( 'Real93:  ' , RA <= RD : 5 , ' s/b true' ) ;
  WRITELN ( 'Real94:  ' , RB <= RA : 5 , ' s/b true' ) ;
  WRITELN ( 'Real95:  ' , RA <= RC : 5 , ' s/b true' ) ;
  WRITELN ( 'Real96:  ' , RD <= RA : 5 , ' s/b false' ) ;
  WRITELN ( 'Real97:  ' , RA <= RB : 5 , ' s/b false' ) ;
  WRITELN ( 'Real98:  ' , RD >= RA : 5 , ' s/b true' ) ;
  WRITELN ( 'Real99:  ' , RA >= RB : 5 , ' s/b true' ) ;
  WRITELN ( 'Real100: ' , RA >= RC : 5 , ' s/b true' ) ;
  WRITELN ( 'Real101: ' , RA >= RD : 5 , ' s/b false' ) ;
  WRITELN ( 'Real102: ' , RB >= RA : 5 , ' s/b false' ) ;
  WRITELN ( 'Real103: ' , ABS ( RA ) : 15 , ' s/b  7.34200e+02' ) ;
  WRITELN ( 'Real104: ' , SQR ( RA ) : 15 , ' s/b  5.39050e+05' ) ;
  WRITELN ( 'Real105: ' , SIN ( RB ) : 15 , ' s/b -4.34850e-01' ) ;
  WRITELN ( 'Real106: ' , ARCTAN ( RA ) : 15 , ' s/b -1.56943e+00' ) ;
  WRITELN ( 'Real107: ' , EXP ( RE ) : 15 , ' s/b  6.80566e-01' ) ;
  WRITELN ( 'Real108: ' , TRUNC ( RA ) : 15 , ' s/b -734' ) ;
  WRITELN ( 'Real109: ' , ROUND ( RB ) : 15 , ' s/b -7635' ) ;
  WRITELN ( 'Real110: ' , ROUND ( RA ) : 15 , ' s/b -734' ) ;

  (********************)
  (* signed constants *)
  (********************)

  WRITELN ( 'Real111: ' , 45.934 + ( - 30.834 ) : 15 ,
            ' s/b  1.510000e+01' ) ;
  WRITELN ( 'Real112: ' , - 25.737 + 70.87 : 15 , ' s/b  4.513300e+01'
            ) ;
  WRITELN ( 'Real113: ' , - 62.63 + 23.99 : 15 , ' s/b -3.864000e+01' )
            ;
  WRITELN ( 'Real114: ' , - 20.733 + ( - 15.848 ) : 15 ,
            ' s/b -3.658100e+01' ) ;
  WRITELN ( 'Real115: ' , 20.774 - ( - 14.774 ) : 15 ,
            ' s/b  3.554800e+01' ) ;
  WRITELN ( 'Real116: ' , - 34.523 - 14.8754 : 15 ,
            ' s/b -4.939840e+01' ) ;
  WRITELN ( 'Real117: ' , - 56.664 - ( - 12.663 ) : 15 ,
            ' s/b -4.400100e+01' ) ;
  WRITELN ( 'Real118: ' , 5.663 * ( - 4.664 ) : 15 ,
            ' s/b -2.641223e+01' ) ;
  WRITELN ( 'Real119: ' , ( - 18.62 ) * 7.997 : 15 ,
            ' s/b -1.489041e+02' ) ;
  WRITELN ( 'Real120: ' , ( - 40.552 ) * ( - 13.774 ) : 15 ,
            ' s/b  5.585632e+02' ) ;
  WRITELN ( 'Real121: ' , 30.6632 / ( - 5.874 ) : 15 ,
            ' s/b -5.220157e+00' ) ;
  WRITELN ( 'Real122: ' , ( - 50.636 ) / 2.8573 : 15 ,
            ' s/b -1.772163e+01' ) ;
  WRITELN ( 'Real123: ' , ( - 20.7631 ) / ( - 4.85734 ) : 15 ,
            ' s/b  4.274582e+00' ) ;
  WRITELN ( 'Real124: ' , - 5.775 = - 5.775 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real125: ' , - 5.6364 = 5.8575 : 5 , ' s/b false' ) ;
  WRITELN ( 'Real126: ' , - 21.6385 <> - 40.764 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real127: ' , - 21.772 <> - 21.772 : 5 , ' s/b false' ) ;
  WRITELN ( 'Real128: ' , - 3.512 < 5.8467 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real129: ' , - 32.644 < - 20.9074 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real130: ' , 20.763 < - 20.743 : 5 , ' s/b false' ) ;
  WRITELN ( 'Real131: ' , - 15.663 < - 40.784 : 5 , ' s/b false' ) ;
  WRITELN ( 'Real132: ' , 70.766 > - 4.974 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real133: ' , - 23.6532 > - 34.774 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real134: ' , - 5.773 > 5.9874 : 5 , ' s/b false' ) ;
  WRITELN ( 'Real135: ' , - 60.663 > - 59.78 : 5 , ' s/b false' ) ;
  WRITELN ( 'Real136: ' , - 12.542 <= 4.0848 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real137: ' , - 14.8763 <= - 5.0847 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real138: ' , - 7.8373 <= - 7.8373 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real139: ' , 5.4564 <= - 5.4564 : 5 , ' s/b false' ) ;
  WRITELN ( 'Real140: ' , - 10.72633 <= - 20.984 : 5 , ' s/b false' ) ;
  WRITELN ( 'Real141: ' , 9.834 >= - 3.9383 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real142: ' , - 4.562 >= - 10.74 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real143: ' , - 13.63 >= - 13.63 : 5 , ' s/b true' ) ;
  WRITELN ( 'Real144: ' , - 6.74 >= 6.74 : 5 , ' s/b false' ) ;
  WRITELN ( 'Real145: ' , - 20.7623 >= - 10.574 : 5 , ' s/b false' ) ;
  WRITELN ( 'Real146: ' , ABS ( - 6.823 ) : 15 , ' s/b  6.823000e+00' )
            ;
  WRITELN ( 'Real147  ' , SQR ( - 348.22 ) : 15 , ' s/b  1.212572e+05'
            ) ;
  WRITELN ( 'Real148: ' , SIN ( - 733.22 ) : 15 , ' s/b  9.421146e-01'
            ) ;
  WRITELN ( 'Real149: ' , ARCTAN ( - 8387.22 ) : 15 ,
            ' s/b -1.570677e+00' ) ;
  WRITELN ( 'Real150: ' , EXP ( - 0.8743 ) : 15 , ' s/b  4.171539e-01'
            ) ;
  WRITELN ( 'Real151: ' , TRUNC ( - 33.422 ) : 1 , ' s/b -33' ) ;
  WRITELN ( 'Real152: ' , ROUND ( - 843.22 ) : 1 , ' s/b -843' ) ;
  WRITELN ( 'Real153: ' , ROUND ( - 6243.76 ) : 1 , ' s/b -6244' ) ;
  WRITELN ( 'Real154: ' , RSCST : 15 , ' s/b -8.422000e+01' ) ;
  WRITELN ( 'Real155: ' , - RSCST : 15 , ' s/b  8.422000e+01' ) ;
  WRITELN ( 'Real156:  ' , RSCST2 : 15 , ' s/b -4.333000e+01' ) ;
  WRITELN ( 'Real157: ' , RSCST3 : 15 , ' s/b  8.422000e+01' ) ;

  (*****************************************************************)
  (*                                                               *)
  (*                         Sets                                  *)
  (*                                                               *)
  (*****************************************************************)

  WRITELN ;
  WRITELN ( '******************* sets ******************************' )
            ;
  WRITELN ;

  (********************)
  (* sets of integers *)
  (********************)

  WRITE ( 'Set1:  ' ) ;
  STA := [ ] ;
  for I := 1 to 10 do
    if ODD ( I ) then
      STA := STA + [ I , I + 10 ] ;
  for I := 1 to 20 do
    if I in STA then
      WRITE ( '1' )
    else
      WRITE ( '0' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( '10101010101010101010' ) ;
  WRITE ( 'Set2:  ' ) ;
  STA := [ 1 , 4 , 5 ] ;
  STB := [ 2 , 6 , 10 ] ;
  for I := 1 to 10 do
    if I in STA + STB then
      WRITE ( '1' )
    else
      WRITE ( '0' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( '1101110001' ) ;
  WRITE ( 'Set3:  ' ) ;
  STA := [ 1 , 2 , 6 , 5 , 7 ] ;
  STB := [ 2 , 6 , 10 ] ;
  for I := 1 to 10 do
    if I in STA * STB then
      WRITE ( '1' )
    else
      WRITE ( '0' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( '0100010000' ) ;
  WRITE ( 'Set4:  ' ) ;
  STA := [ 2 , 4 , 7 , 8 ] ;
  STB := [ 1 , 3 , 4 , 8 , 10 ] ;
  for I := 1 to 10 do
    if I in STA - STB then
      WRITE ( '1' )
    else
      WRITE ( '0' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( '0100001000' ) ;
  STA := [ 4 , 6 , 8 , 9 ] ;
  STB := [ 1 , 4 , 5 , 9 ] ;
  STC := [ 4 , 6 , 8 , 9 ] ;
  WRITELN ( 'Set5:  ' , STA = STB : 5 , ' s/b false' ) ;
  WRITELN ( 'Set6:  ' , STA = STC : 5 , ' s/b true' ) ;
  WRITELN ( 'Set7:  ' , STA <> STB : 5 , ' s/b true' ) ;
  WRITELN ( 'Set8:  ' , STA <> STC : 5 , ' s/b false' ) ;
  STA := [ 1 , 2 , 5 , 7 , 10 ] ;
  STB := [ 1 , 5 , 10 ] ;
  STC := [ 1 , 5 , 10 , 6 ] ;
  STD := [ 1 , 2 , 5 , 7 , 10 ] ;
  WRITELN ( 'Set9:  ' , STB <= STA : 5 , ' s/b true' ) ;
  WRITELN ( 'Set10: ' , STB <= STD : 5 , ' s/b true' ) ;
  WRITELN ( 'Set11: ' , STC <= STA : 5 , ' s/b false' ) ;
  WRITELN ( 'Set12: ' , STA >= STB : 5 , ' s/b true' ) ;
  WRITELN ( 'Set13: ' , STD >= STB : 5 , ' s/b true' ) ;
  WRITELN ( 'Set14: ' , STA >= STC : 5 , ' s/b false' ) ;
  WRITE ( 'Set15: ' ) ;
  I := 2 ;
  X := 4 ;
  STA := [ I , X , I + X ] ;
  for I := 1 to 10 do
    if I in STA then
      WRITE ( '1' )
    else
      WRITE ( '0' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( '0101010000' ) ;

  (*************************************)
  (* these are just compile time tests *)
  (*************************************)

  STE := STD ;
  STF := [ 1 , 2 , 5 , 7 ] ;
  STG := STF ;

  (**********************)
  (* sets of characters *)
  (**********************)

  WRITE ( 'Set16: ' ) ;
  CSTA := [ ] ;
  for CI := 'a' to 'j' do
    if ODD ( ORD ( CI ) ) then
      CSTA := CSTA + [ CI , CHR ( ORD ( CI ) + 10 ) ] ;
  for CI := 'a' to 't' do
    if CI in CSTA then
      WRITE ( CI )
    else
      WRITE ( '_' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( 'a_c_e_g_i_k_m_o_q_s_' ) ;
  WRITE ( 'Set17: ' ) ;
  CSTA := [ 'a' , 'c' , 'f' ] ;
  CSTB := [ 'c' , 'd' , 'g' ] ;
  for CI := 'a' to 'j' do
    if CI in CSTA + CSTB then
      WRITE ( CI )
    else
      WRITE ( '_' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( 'a_cd_fg___' ) ;
  WRITE ( 'Set18: ' ) ;
  CSTA := [ 'd' , 'f' , 'h' , 'a' ] ;
  CSTB := [ 'a' , 'b' , 'i' , 'h' ] ;
  for CI := 'a' to 'j' do
    if CI in CSTA * CSTB then
      WRITE ( CI )
    else
      WRITE ( '_' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( 'a______h__' ) ;
  WRITE ( 'Set19: ' ) ;
  CSTA := [ 'b' , 'd' , 'i' , 'j' ] ;
  CSTB := [ 'i' , 'h' , 'd' , 'e' ] ;
  for CI := 'a' to 'j' do
    if CI in CSTA - CSTB then
      WRITE ( CI )
    else
      WRITE ( '_' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( '_b_______j' ) ;
  CSTA := [ 'b' , 'd' , 'h' , 'j' ] ;
  CSTB := [ 'a' , 'd' , 'h' , 'c' ] ;
  CSTC := [ 'b' , 'd' , 'h' , 'j' ] ;
  WRITELN ( 'Set20: ' , CSTA = CSTB : 5 , ' s/b false' ) ;
  WRITELN ( 'Set21: ' , CSTA = CSTC : 5 , ' s/b true' ) ;
  WRITELN ( 'Set22: ' , CSTA <> CSTB : 5 , ' s/b true' ) ;
  WRITELN ( 'Set23: ' , CSTA <> CSTC : 5 , ' s/b false' ) ;
  CSTA := [ 'a' , 'b' , 'f' , 'g' , 'j' ] ;
  CSTB := [ 'a' , 'f' , 'g' ] ;
  CSTC := [ 'a' , 'f' , 'g' , 'h' ] ;
  CSTD := [ 'a' , 'b' , 'f' , 'g' , 'j' ] ;
  WRITELN ( 'Set24: ' , CSTB <= CSTA : 5 , ' s/b true' ) ;
  WRITELN ( 'Set25: ' , CSTB <= CSTD : 5 , ' s/b true' ) ;
  WRITELN ( 'Set26: ' , CSTC <= CSTA : 5 , ' s/b false' ) ;
  WRITELN ( 'Set27: ' , CSTA >= CSTB : 5 , ' s/b true' ) ;
  WRITELN ( 'Set28: ' , CSTD >= CSTB : 5 , ' s/b true' ) ;
  WRITELN ( 'Set29: ' , CSTA >= CSTC : 5 , ' s/b false' ) ;
  WRITE ( 'Set30: ' ) ;
  CI := 'a' ;
  I := 4 ;
  CSTA := [ CI , CHR ( ORD ( CI ) + I ) ] ;
  for CI := 'a' to 'j' do
    if CI in CSTA then
      WRITE ( CI )
    else
      WRITE ( '_' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( 'a___e_____' ) ;

  (*************************************)
  (* these are just compile time tests *)
  (*************************************)

  CSTE := CSTD ;
  CSTF := [ 'a' , 'b' , 'e' , 'f' ] ;
  CSTG := CSTF ;

  (**********************)
  (* sets of enumerated *)
  (**********************)

  WRITE ( 'Set31: ' ) ;
  SENA := [ ] ;
  for EI := ONE to TEN do
    if ODD ( ORD ( EI ) ) then
      SENA := SENA + [ EI ] ;
  for EI := ONE to TEN do
    if EI in SENA then
      WRITE ( '1' )
    else
      WRITE ( '0' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( '0101010101' ) ;
  WRITE ( 'Set32: ' ) ;
  SENA := [ ONE , FOUR , FIVE ] ;
  SENB := [ TWO , SIX , TEN ] ;
  for EI := ONE to TEN do
    if EI in SENA + SENB then
      WRITE ( '1' )
    else
      WRITE ( '0' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( '1101110001' ) ;
  WRITE ( 'Set33: ' ) ;
  SENA := [ ONE , TWO , SIX , FIVE , SEVEN ] ;
  SENB := [ TWO , SIX , TEN ] ;
  for EI := ONE to TEN do
    if EI in SENA * SENB then
      WRITE ( '1' )
    else
      WRITE ( '0' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( '0100010000' ) ;
  WRITE ( 'Set34: ' ) ;
  SENA := [ TWO , FOUR , SEVEN , EIGHT ] ;
  SENB := [ ONE , THREE , FOUR , EIGHT , TEN ] ;
  for EI := ONE to TEN do
    if EI in SENA - SENB then
      WRITE ( '1' )
    else
      WRITE ( '0' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( '0100001000' ) ;
  SENA := [ FOUR , SIX , EIGHT , NINE ] ;
  SENB := [ ONE , FOUR , FIVE , NINE ] ;
  SENC := [ FOUR , SIX , EIGHT , NINE ] ;
  WRITELN ( 'Set35: ' , SENA = SENB : 5 , ' s/b false' ) ;
  WRITELN ( 'Set36: ' , SENA = SENC : 5 , ' s/b true' ) ;
  WRITELN ( 'Set37: ' , SENA <> SENB : 5 , ' s/b true' ) ;
  WRITELN ( 'Set38: ' , SENA <> SENC : 5 , ' s/b false' ) ;
  SENA := [ ONE , TWO , FIVE , SEVEN , TEN ] ;
  SENB := [ ONE , FIVE , TEN ] ;
  SENC := [ ONE , FIVE , TEN , SIX ] ;
  SEND := [ ONE , TWO , FIVE , SEVEN , TEN ] ;
  WRITELN ( 'Set39: ' , SENB <= SENA : 5 , ' s/b true' ) ;
  WRITELN ( 'Set40: ' , SENB <= SEND : 5 , ' s/b true' ) ;
  WRITELN ( 'Set41: ' , SENC <= SENA : 5 , ' s/b false' ) ;
  WRITELN ( 'Set42: ' , SENA >= SENB : 5 , ' s/b true' ) ;
  WRITELN ( 'Set43: ' , SEND >= SENB : 5 , ' s/b true' ) ;
  WRITELN ( 'Set44: ' , SENA >= SENC : 5 , ' s/b false' ) ;
  WRITE ( 'Set45: ' ) ;
  EI := TWO ;
  SENA := [ EI , SUCC ( EI ) ] ;
  for EI := ONE to TEN do
    if EI in SENA then
      WRITE ( '1' )
    else
      WRITE ( '0' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( '0110000000' ) ;

  (*************************************)
  (* these are just compile time tests *)
  (*************************************)

  SEND := [ ONE , TWO , FIVE ] ;
  SENE := SEND ;
  SENF := [ ONE , TWO , FIVE , SEVEN ] ;
  SENG := SENF ;

  (*******************)
  (* sets of boolean *)
  (*******************)

  WRITE ( 'Set46: ' ) ;
  SBA := [ ] ;
  for BA := FALSE to TRUE do
    if ODD ( ORD ( BA ) ) then
      SBA := SBA + [ BA ] ;
  for BA := FALSE to TRUE do
    if BA in SBA then
      WRITE ( '1' )
    else
      WRITE ( '0' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( '01' ) ;
  WRITE ( 'Set47: ' ) ;
  SBA := [ FALSE ] ;
  SBB := [ TRUE ] ;
  for BA := FALSE to TRUE do
    if BA in SBA + SBB then
      WRITE ( '1' )
    else
      WRITE ( '0' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( '11' ) ;
  WRITE ( 'Set48: ' ) ;
  SBA := [ FALSE , TRUE ] ;
  SBB := [ FALSE ] ;
  for BA := FALSE to TRUE do
    if BA in SBA * SBB then
      WRITE ( '1' )
    else
      WRITE ( '0' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( '10' ) ;
  WRITE ( 'Set49: ' ) ;
  SBA := [ TRUE , FALSE ] ;
  SBB := [ TRUE ] ;
  for BA := FALSE to TRUE do
    if BA in SBA - SBB then
      WRITE ( '1' )
    else
      WRITE ( '0' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( '10' ) ;
  SBA := [ TRUE ] ;
  SBB := [ FALSE ] ;
  SBC := [ TRUE ] ;
  WRITELN ( 'Set50: ' , SBA = SBB : 5 , ' s/b false' ) ;
  WRITELN ( 'Set51: ' , SBA = SBC : 5 , ' s/b true' ) ;
  WRITELN ( 'Set52: ' , SBA <> SBB : 5 , ' s/b true' ) ;
  WRITELN ( 'Set53: ' , SBA <> SBC : 5 , ' s/b false' ) ;
  SBA := [ TRUE , FALSE ] ;
  SBB := [ FALSE ] ;
  SBC := [ TRUE ] ;
  SBD := [ FALSE ] ;
  WRITELN ( 'Set54: ' , SBB <= SBA : 5 , ' s/b true' ) ;
  WRITELN ( 'Set55: ' , SBB <= SBD : 5 , ' s/b true' ) ;
  WRITELN ( 'Set56: ' , SBC <= SBB : 5 , ' s/b false' ) ;
  WRITELN ( 'Set57: ' , SBA >= SBB : 5 , ' s/b true' ) ;
  WRITELN ( 'Set58: ' , SBD >= SBB : 5 , ' s/b true' ) ;
  WRITELN ( 'Set59: ' , SBB >= SBC : 5 , ' s/b false' ) ;
  WRITE ( 'Set60: ' ) ;
  BA := FALSE ;
  SBA := [ BA , SUCC ( BA ) ] ;
  for BA := FALSE to TRUE do
    if BA in SBA then
      WRITE ( '1' )
    else
      WRITE ( '0' ) ;
  WRITE ( ' s/b ' ) ;
  WRITELN ( '11' ) ;

  (*************************************)
  (* these are just compile time tests *)
  (*************************************)

  SBE := SBD ;
  SBF := [ TRUE ] ;
  SBG := SBF ;
  WRITE ( 'set61: ' ) ;
  NEW ( PI1 ) ;
  NEW ( PI2 ) ;
  PI1 -> := 3 ;
  PI2 -> := 5 ;
  WRITE ( [ PI1 -> .. PI2 -> ] = [ 3 .. 5 ] : 5 ) ;
  WRITELN ( ' s/b true' ) ;

  (*****************************************************************)
  (*                                                               *)
  (*                         Pointers                              *)
  (*                                                               *)
  (*****************************************************************)

  WRITELN ;
  WRITELN (
         '******************* Pointers ******************************'
            ) ;
  WRITELN ;

  (*********************)
  (* pointers to types *)
  (*********************)

  WRITE ( 'Pointer1:   ' ) ;
  NEW ( PTI ) ;
  PTI -> := 4594 ;
  WRITELN ( PTI -> : 1 , ' s/b 4594' ) ;
  WRITE ( 'Pointer2:   ' ) ;
  NEW ( PTB ) ;
  PTB -> := TRUE ;
  WRITELN ( PTB -> : 5 , ' s/b  true' ) ;
  WRITE ( 'Pointer3:   ' ) ;
  NEW ( PTB ) ;
  PTB -> := FALSE ;
  WRITELN ( PTB -> : 5 , ' s/b false' ) ;
  WRITE ( 'Pointer4:   ' ) ;
  NEW ( PTC ) ;
  PTC -> := 'p' ;
  WRITELN ( PTC -> , ' s/b p' ) ;
  WRITE ( 'Pointer5:   ' ) ;
  NEW ( PTE ) ;
  PTE -> := SIX ;
  WRITELN ( ORD ( PTE -> ) : 1 , ' s/b 5' ) ;
  WRITE ( 'Pointer6:   ' ) ;
  NEW ( PTES ) ;
  PTES -> := FOUR ;
  WRITELN ( ORD ( PTES -> ) : 1 , ' s/b 3' ) ;
  WRITE ( 'Pointer7:   ' ) ;
  NEW ( PTS ) ;
  PTS -> := 17 ;
  WRITELN ( PTS -> : 1 , ' s/b 17' ) ;
  WRITE ( 'Pointer8:   ' ) ;
  NEW ( PTR ) ;
  PTR -> := 1234.5678 ;
  WRITELN ( PTR -> : 1 : 4 , ' s/b 1234.5678' ) ;
  WRITE ( 'Pointer9:   ' ) ;
  NEW ( PTST ) ;
  PTST -> := 'my word is' ;
  WRITELN ( PTST -> , ' s/b my word is' ) ;
  WRITE ( 'Pointer10:  ' ) ;
  NEW ( PTA ) ;
  for I := 1 to 10 do
    PTA -> [ I ] := I + 10 ;
  for I := 10 DOWNTO 1 do
    WRITE ( PTA -> [ I ] : 1 , ' ' ) ;
  WRITELN ( 's/b 20 19 18 17 16 15 14 13 12 11' ) ;
  WRITE ( 'Pointer11:   ' ) ;
  NEW ( PTRC ) ;
  PTRC -> . A := 7234 ;
  PTRC -> . B := 'y' ;
  WRITELN ( PTRC -> . A : 1 , ' ' , PTRC -> . B , ' s/b 7234 y' ) ;
  WRITE ( 'Pointer12:   ' ) ;
  NEW ( PTSTC ) ;
  PTSTC -> := [ 'b' , 'd' , 'i' .. 'j' ] ;
  for CI := 'a' to 'j' do
    if CI in PTSTC -> then
      WRITE ( CI )
    else
      WRITE ( '_' ) ;
  WRITELN ( ' s/b _b_d____ij' ) ;
  WRITE ( 'Pointer13:  ' ) ;
  NEW ( PTP ) ;
  NEW ( PTP -> ) ;
  PTP -> -> := 3732 ;
  WRITELN ( PTP -> -> : 1 , ' s/b 3732' ) ;

  (****************************)
  (* equality/inequality, nil *)
  (****************************)

  WRITE ( 'Pointer14:  ' ) ;
  PTI := NIL ;
  WRITELN ( PTI = NIL : 5 , ' s/b  true' ) ;
  WRITE ( 'Pointer15:  ' ) ;
  NEW ( PTI ) ;
  WRITELN ( PTI = NIL : 5 , ' s/b false' ) ;
  WRITE ( 'Pointer16:  ' ) ;
  PTI1 := PTI ;
  WRITELN ( PTI = PTI1 : 5 , ' s/b true' ) ;
  WRITE ( 'Pointer17:  ' ) ;
  PTI1 := PTI ;
  WRITELN ( PTI <> PTI1 : 5 , ' s/b false' ) ;
  WRITE ( 'Pointer18:  ' ) ;
  NEW ( PTI1 ) ;
  WRITELN ( PTI = PTI1 : 5 , ' s/b false' ) ;
  WRITE ( 'Pointer19:  ' ) ;
  WRITELN ( PTI <> PTI1 : 5 , ' s/b  true' ) ;

  (***********************************)
  (* dynamic allocation stress tests *)
  (* allocate top to bottom, then fr *)
  (*ee from top to bottom            *)
  (***********************************)

  WRITE ( 'Pointer20:  ' ) ;
  NEW ( IPA ) ;
  NEW ( IPB ) ;
  NEW ( IPC ) ;
  DISPOSE ( IPA ) ;
  DISPOSE ( IPB ) ;
  DISPOSE ( IPC ) ;
  WRITELN ( 'done s/b done' ) ;

  (********************************************************)
  (* allocate top to bottom, then free from bottom to top *)
  (********************************************************)

  WRITE ( 'Pointer21:  ' ) ;
  NEW ( IPA ) ;
  NEW ( IPB ) ;
  NEW ( IPC ) ;
  DISPOSE ( IPC ) ;
  DISPOSE ( IPB ) ;
  DISPOSE ( IPA ) ;

  (*****************************************)
  (* free 2 middle blocks to test coalesce *)
  (*****************************************)

  WRITE ( 'Pointer22:  ' ) ;
  NEW ( IPA ) ;
  NEW ( IPB ) ;
  NEW ( IPC ) ;
  NEW ( IPD ) ;
  DISPOSE ( IPB ) ;
  DISPOSE ( IPC ) ;
  DISPOSE ( IPA ) ;
  DISPOSE ( IPD ) ;
  WRITELN ( 'done s/b done' ) ;

  (*****************************************)
  (* free 3 middle blocks to test coalesce *)
  (*****************************************)

  WRITE ( 'Pointer23:  ' ) ;
  NEW ( IPA ) ;
  NEW ( IPB ) ;
  NEW ( IPC ) ;
  NEW ( IPD ) ;
  NEW ( IPE ) ;
  DISPOSE ( IPB ) ;
  DISPOSE ( IPD ) ;
  DISPOSE ( IPC ) ;
  DISPOSE ( IPA ) ;
  DISPOSE ( IPE ) ;
  WRITELN ( 'done s/b done' ) ;

  (***********************)
  (* linear torture test *)
  (***********************)

  WRITELN ( 'Pointer24:  ' ) ;
  for CNT := 1 to 100 do
    begin
      WRITE ( CNT : 3 , ' ' ) ;
      if ( CNT MOD 10 ) = 0 then
        WRITELN ;
      for I := 1 to 100 do
        IAP [ I ] := NIL ;
      for I := 1 to 100 do
        begin
          NEW ( IAP [ I ] ) ;
          IAP [ I ] -> := I
        end (* for *) ;
      for I := 1 to 100 do
        if IAP [ I ] = NIL then
          WRITELN ( '*** bad allocation of block' ) ;
      for I := 100 DOWNTO 1 do
        if IAP [ I ] -> <> I then
          WRITELN ( '*** bad block content' ) ;
      for I := 1 to 100 do
        begin
          DISPOSE ( IAP [ I ] ) ;
          IAP [ I ] := NIL ;
          for X := 1 to 100 do
            if IAP [ X ] <> NIL then
              if IAP [ X ] -> <> X then
                WRITELN ( '*** bad block content' )
        end (* for *) ;
      for I := 1 to 100 do
        IAP [ I ] := NIL ;
      for I := 1 to 100 do
        begin
          NEW ( IAP [ I ] ) ;
          IAP [ I ] -> := I
        end (* for *) ;
      for I := 1 to 100 do
        if IAP [ I ] = NIL then
          WRITELN ( '*** bad allocation of block' ) ;
      for I := 100 DOWNTO 1 do
        if IAP [ I ] -> <> I then
          WRITELN ( '*** bad block content' ) ;
      for I := 100 DOWNTO 1 do
        begin
          DISPOSE ( IAP [ I ] ) ;
          IAP [ I ] := NIL ;
          for X := 1 to 100 do
            if IAP [ X ] <> NIL then
              if IAP [ X ] -> <> X then
                WRITELN ( '*** bad block content' )
        end (* for *)
    end (* for *) ;
  WRITELN ;
  WRITELN ( 's/b' ) ;
  WRITELN ;
  WRITELN ( '  1   2   3   4   5   6   7   8   9  10' ) ;
  WRITELN ( ' 11  12  13  14  15  16  17  18  19  20' ) ;
  WRITELN ( ' 21  22  23  24  25  26  27  28  29  30' ) ;
  WRITELN ( ' 31  32  33  34  35  36  37  38  39  40' ) ;
  WRITELN ( ' 41  42  43  44  45  46  47  48  49  50' ) ;
  WRITELN ( ' 51  52  53  54  55  56  57  58  59  60' ) ;
  WRITELN ( ' 61  62  63  64  65  66  67  68  69  70' ) ;
  WRITELN ( ' 71  72  73  74  75  76  77  78  79  80' ) ;
  WRITELN ( ' 81  82  83  84  85  86  87  88  89  90' ) ;
  WRITELN ( ' 91  92  93  94  95  96  97  98  99  100' ) ;
  RNDSEQ := 1 ;

  (*****************************)
  (* random block torture test *)
  (*****************************)

  WRITELN ( 'Pointer25:  ' ) ;
  for I := 1 to 100 do
    IAP [ I ] := NIL ;
  for CNT2 := 1 to 100 do
    begin
      WRITE ( CNT2 : 3 , ' ' ) ;
      if ( CNT2 MOD 10 ) = 0 then
        WRITELN ;
      for CNT := 1 to 100 do
        begin

  (*******************)
  (* allocate random *)
  (*******************)

          RN := RANDOM ( 1 , 100 ) ;

  (*************************)
  (* choose random pointer *)
  (*************************)

          NEW ( IAP [ RN ] ) ;

  (************)
  (* allocate *)
  (************)

          IAP [ RN ] -> := RN ;

  (**************)
  (* set number *)
  (**************)

          for I := 1 to 100 do
            if IAP [ I ] <> NIL then
              if IAP [ I ] -> <> I then
                WRITELN ( '*** bad block content' ) ;

  (*********************)
  (* deallocate random *)
  (*********************)

          RN := RANDOM ( 1 , 100 ) ;

  (*************************)
  (* choose random pointer *)
  (*************************)

          if IAP [ RN ] <> NIL then
            DISPOSE ( IAP [ RN ] ) ;

  (**************)
  (* deallocate *)
  (**************)

          IAP [ RN ] := NIL ;
          for I := 1 to 100 do
            if IAP [ I ] <> NIL then
              if IAP [ I ] -> <> I then
                WRITELN ( '*** bad block content' ) ;
        end (* for *)
    end (* for *) ;
  WRITELN ;
  WRITELN ( 's/b' ) ;
  WRITELN ;
  WRITELN ( '  1   2   3   4   5   6   7   8   9  10' ) ;
  WRITELN ( ' 11  12  13  14  15  16  17  18  19  20' ) ;
  WRITELN ( ' 21  22  23  24  25  26  27  28  29  30' ) ;
  WRITELN ( ' 31  32  33  34  35  36  37  38  39  40' ) ;
  WRITELN ( ' 41  42  43  44  45  46  47  48  49  50' ) ;
  WRITELN ( ' 51  52  53  54  55  56  57  58  59  60' ) ;
  WRITELN ( ' 61  62  63  64  65  66  67  68  69  70' ) ;
  WRITELN ( ' 71  72  73  74  75  76  77  78  79  80' ) ;
  WRITELN ( ' 81  82  83  84  85  86  87  88  89  90' ) ;
  WRITELN ( ' 91  92  93  94  95  96  97  98  99  100' ) ;

  (*****************************************************************)
  (*                                                               *)
  (*                         Arrays                                *)
  (*                                                               *)
  (*****************************************************************)

  WRITELN ;
  WRITELN ( '******************* arrays ******************************'
            ) ;
  WRITELN ;

  (***********************************)
  (* single demension, integer index *)
  (***********************************)

  WRITE ( 'Array1:   ' ) ;
  for I := 1 to 10 do
    AVI [ I ] := I + 10 ;
  for I := 10 DOWNTO 1 do
    WRITE ( AVI [ I ] : 1 , ' ' ) ;
  WRITELN ( ' s/b 20 19 18 17 16 15 14 13 12 11' ) ;
  WRITE ( 'Array2:   ' ) ;
  for I := 1 to 10 do
    PAVI [ I ] := I + 10 ;
  for I := 10 DOWNTO 1 do
    WRITE ( PAVI [ I ] : 1 , ' ' ) ;
  WRITELN ( ' s/b 20 19 18 17 16 15 14 13 12 11' ) ;
  WRITE ( 'Array3:   ' ) ;
  for I := 1 to 10 do
    AVIS [ I ] := I + 10 ;
  for I := 10 DOWNTO 1 do
    WRITE ( AVIS [ I ] : 1 , ' ' ) ;
  WRITELN ( ' s/b 20 19 18 17 16 15 14 13 12 11' ) ;
  WRITE ( 'Array4:   ' ) ;
  for I := 1 to 10 do
    PAVIS [ I ] := I + 10 ;
  for I := 10 DOWNTO 1 do
    WRITE ( PAVIS [ I ] : 1 , ' ' ) ;
  WRITELN ( ' s/b 20 19 18 17 16 15 14 13 12 11' ) ;
  WRITE ( 'Array5:   ' ) ;
  for I := 1 to 10 do
    AVB [ I ] := ODD ( I ) ;
  for I := 10 DOWNTO 1 do
    WRITE ( AVB [ I ] : 5 , ' ' ) ;
  WRITELN ;
  WRITELN (
    '    s/b:   false  true false  true false  true false  true false'
            , '  true' ) ;
  WRITE ( 'Array6:   ' ) ;
  for I := 1 to 10 do
    PAVB [ I ] := ODD ( I ) ;
  for I := 10 DOWNTO 1 do
    WRITE ( PAVB [ I ] : 5 , ' ' ) ;
  WRITELN ;
  WRITELN (
    '    s/b:   false  true false  true false  true false  true false'
            , '  true' ) ;
  WRITE ( 'Array7:   ' ) ;
  for I := 1 to 10 do
    AVR [ I ] := I + 10 + 0.12 ;
  for I := 10 DOWNTO 1 do
    WRITE ( AVR [ I ] : 1 : 2 , ' ' ) ;
  WRITELN ;
  WRITELN ( '    s/b:   20.12 19.12 18.12 17.12 16.12 15.12 14.12 ' ,
            '13.12 12.12 11.12' ) ;
  WRITE ( 'Array8:   ' ) ;
  for I := 1 to 10 do
    PAVR [ I ] := I + 10 + 0.12 ;
  for I := 10 DOWNTO 1 do
    WRITE ( PAVR [ I ] : 1 : 2 , ' ' ) ;
  WRITELN ;
  WRITELN ( '    s/b:   20.12 19.12 18.12 17.12 16.12 15.12 14.12 ' ,
            '13.12 12.12 11.12' ) ;
  WRITE ( 'Array9:   ' ) ;
  for I := 1 to 10 do
    AVC [ I ] := CHR ( I + ORD ( 'a' ) ) ;
  for I := 10 DOWNTO 1 do
    WRITE ( AVC [ I ] : 1 , ' ' ) ;
  WRITELN ( 's/b k j i h g f e d c b' ) ;
  WRITE ( 'Array10:  ' ) ;
  for I := 1 to 10 do
    PAVC [ I ] := CHR ( I + ORD ( 'a' ) ) ;
  for I := 10 DOWNTO 1 do
    WRITE ( PAVC [ I ] : 1 , ' ' ) ;
  WRITELN ( 's/b k j i h g f e d c b' ) ;
  WRITE ( 'Array11:  ' ) ;
  for I := 1 to 10 do
    AVCS [ I ] := CHR ( I + ORD ( 'f' ) ) ;
  for I := 10 DOWNTO 1 do
    WRITE ( AVCS [ I ] : 1 , ' ' ) ;
  WRITELN ( 's/b p o n m l k j i h g' ) ;
  WRITE ( 'Array12:  ' ) ;
  for I := 1 to 10 do
    PAVCS [ I ] := CHR ( I + ORD ( 'f' ) ) ;
  for I := 10 DOWNTO 1 do
    WRITE ( PAVCS [ I ] : 1 , ' ' ) ;
  WRITELN ( 's/b p o n m l k j i h g' ) ;
  WRITE ( 'Array13:  ' ) ;
  for EI := ONE to TEN do
    AVE [ ORD ( EI ) + 1 ] := EI ;
  for EI := TEN DOWNTO ONE do
    WRITE ( ORD ( AVE [ ORD ( EI ) + 1 ] ) : 1 , ' ' ) ;
  WRITELN ( 's/b 9 8 7 6 5 4 3 2 1 0' ) ;
  WRITE ( 'Array14:  ' ) ;
  for EI := ONE to TEN do
    PAVE [ ORD ( EI ) + 1 ] := EI ;
  for EI := TEN DOWNTO ONE do
    WRITE ( ORD ( AVE [ ORD ( EI ) + 1 ] ) : 1 , ' ' ) ;
  WRITELN ( 's/b 9 8 7 6 5 4 3 2 1 0' ) ;
  WRITE ( 'Array15:  ' ) ;
  for EI := THREE to SIX do
    AVES [ ORD ( EI ) + 1 ] := EI ;
  for EI := SIX DOWNTO THREE do
    WRITE ( ORD ( AVES [ ORD ( EI ) + 1 ] ) : 1 , ' ' ) ;
  WRITELN ( 's/b 5 4 3 2' ) ;
  WRITE ( 'Array16:  ' ) ;
  for EI := THREE to SIX do
    PAVES [ ORD ( EI ) + 1 ] := EI ;
  for EI := SIX DOWNTO THREE do
    WRITE ( ORD ( PAVES [ ORD ( EI ) + 1 ] ) : 1 , ' ' ) ;
  WRITELN ( 's/b 5 4 3 2' ) ;
  WRITE ( 'Array17:  ' ) ;
  for I := 1 to 10 do
    AVS [ I ] := [ CHR ( I + ORD ( 'a' ) ) ] ;
  for I := 10 DOWNTO 1 do
    for CI := 'a' to 'z' do
      if CI in AVS [ I ] then
        WRITE ( CI , ' ' ) ;
  WRITELN ( 's/b k j i h g f e d c b' ) ;
  WRITE ( 'Array18:  ' ) ;
  for I := 1 to 10 do
    PAVS [ I ] := [ CHR ( I + ORD ( 'a' ) ) ] ;
  for I := 10 DOWNTO 1 do
    for CI := 'a' to 'z' do
      if CI in PAVS [ I ] then
        WRITE ( CI , ' ' ) ;
  WRITELN ( 's/b k j i h g f e d c b' ) ;
  WRITE ( 'Array19:  ' ) ;
  for I := 1 to 10 do
    begin
      AVRC [ I ] . A := I + 10 ;
      AVRC [ I ] . B := CHR ( I + ORD ( 'a' ) )
    end (* for *) ;
  for I := 10 DOWNTO 1 do
    WRITE ( AVRC [ I ] . A : 1 , ' ' , AVRC [ I ] . B , ' ' ) ;
  WRITELN ;
  WRITELN (
        '     s/b:  20 k 19 j 18 i 17 h 16 g 15 f 14 e 13 d 12 c 11 b'
            ) ;
  WRITE ( 'Array20:  ' ) ;
  for I := 1 to 10 do
    begin
      PAVRC [ I ] . A := I + 10 ;
      PAVRC [ I ] . B := CHR ( I + ORD ( 'a' ) )
    end (* for *) ;
  for I := 10 DOWNTO 1 do
    WRITE ( PAVRC [ I ] . A : 1 , ' ' , PAVRC [ I ] . B , ' ' ) ;
  WRITELN ;
  WRITELN (
        '     s/b:  20 k 19 j 18 i 17 h 16 g 15 f 14 e 13 d 12 c 11 b'
            ) ;
  WRITE ( 'Array21:  ' ) ;
  for I := 1 to 10 do
    begin
      REWRITE ( AVF [ I ] ) ;
      WRITELN ( AVF [ I ] , I + 10 )
    end (* for *) ;
  for I := 10 DOWNTO 1 do
    begin
      RESET ( AVF [ I ] ) ;
      READLN ( AVF [ I ] , X ) ;
      WRITE ( X : 1 , ' ' )
    end (* for *) ;
  WRITELN ( 's/b 20 19 18 17 16 15 14 13 12 11' ) ;
  WRITE ( 'Array22:  ' ) ;
  for I := 1 to 10 do
    begin
      REWRITE ( PAVF [ I ] ) ;
      WRITELN ( PAVF [ I ] , I + 10 )
    end (* for *) ;
  for I := 10 DOWNTO 1 do
    begin
      RESET ( PAVF [ I ] ) ;
      READLN ( PAVF [ I ] , X ) ;
      WRITE ( X : 1 , ' ' )
    end (* for *) ;
  WRITELN ( 's/b 20 19 18 17 16 15 14 13 12 11' ) ;
  WRITE ( 'Array23:  ' ) ;
  for I := 1 to 10 do
    begin
      NEW ( AVP [ I ] ) ;
      AVP [ I ] -> := I + 10
    end (* for *) ;
  for I := 10 DOWNTO 1 do
    WRITE ( AVP [ I ] -> : 1 , ' ' ) ;
  WRITELN ( 's/b 20 19 18 17 16 15 14 13 12 11' ) ;
  WRITE ( 'Array24:  ' ) ;
  for I := 1 to 10 do
    begin
      NEW ( PAVP [ I ] ) ;
      PAVP [ I ] -> := I + 10
    end (* for *) ;
  for I := 10 DOWNTO 1 do
    WRITE ( PAVP [ I ] -> : 1 , ' ' ) ;
  WRITELN ( 's/b 20 19 18 17 16 15 14 13 12 11' ) ;

  (******************)
  (* indexing tests *)
  (******************)

  WRITE ( 'Array25:  ' ) ;
  for BA := FALSE to TRUE do
    BIA [ BA ] := ORD ( BA ) + 10 ;
  for BA := TRUE DOWNTO FALSE do
    WRITE ( BIA [ BA ] : 1 , ' ' ) ;
  WRITELN ( ' s/b 11 10' ) ;
  WRITE ( 'Array26:  ' ) ;
  for BA := FALSE to TRUE do
    PBIA [ BA ] := ORD ( BA ) + 10 ;
  for BA := TRUE DOWNTO FALSE do
    WRITE ( PBIA [ BA ] : 1 , ' ' ) ;
  WRITELN ( ' s/b 11 10' ) ;
  WRITE ( 'Array27:  ' ) ;
  for CI := 'a' to 'j' do
    CIA [ CI ] := ORD ( CI ) ;
  for CI := 'j' DOWNTO 'a' do
    WRITE ( CHR ( CIA [ CI ] ) , ' ' ) ;
  WRITELN ( ' s/b  j i h g f e d c b a' ) ;
  WRITE ( 'Array28:  ' ) ;
  for CI := 'a' to 'j' do
    PCIA [ CI ] := ORD ( CI ) ;
  for CI := 'j' DOWNTO 'a' do
    WRITE ( CHR ( PCIA [ CI ] ) , ' ' ) ;
  WRITELN ( ' s/b  j i h g f e d c b a' ) ;
  WRITE ( 'Array29:  ' ) ;
  for CI := 'a' to 'j' do
    CSIA [ CI ] := ORD ( CI ) ;
  for CI := 'j' DOWNTO 'a' do
    WRITE ( CHR ( CSIA [ CI ] ) , ' ' ) ;
  WRITELN ( ' s/b  j i h g f e d c b a' ) ;
  WRITE ( 'Array30:  ' ) ;
  for CI := 'a' to 'j' do
    PCSIA [ CI ] := ORD ( CI ) ;
  for CI := 'j' DOWNTO 'a' do
    WRITE ( CHR ( PCSIA [ CI ] ) , ' ' ) ;
  WRITELN ( ' s/b  j i h g f e d c b a' ) ;
  WRITE ( 'Array31:  ' ) ;
  for EI := ONE to TEN do
    EIA [ EI ] := ORD ( EI ) ;
  for EI := TEN DOWNTO ONE do
    WRITE ( EIA [ EI ] : 1 , ' ' ) ;
  WRITELN ( ' s/b  9 8 7 6 5 4 3 2 1 0' ) ;
  WRITE ( 'Array32:  ' ) ;
  for EI := ONE to TEN do
    PEIA [ EI ] := ORD ( EI ) ;
  for EI := TEN DOWNTO ONE do
    WRITE ( PEIA [ EI ] : 1 , ' ' ) ;
  WRITELN ( ' s/b  9 8 7 6 5 4 3 2 1 0' ) ;
  WRITE ( 'Array33:  ' ) ;
  for EI := TWO to SIX do
    EIA [ EI ] := ORD ( EI ) ;
  for EI := SIX DOWNTO TWO do
    WRITE ( EIA [ EI ] : 1 , ' ' ) ;
  WRITELN ( ' s/b  5 4 3 2 1' ) ;
  WRITE ( 'Array34:  ' ) ;
  for EI := TWO to SIX do
    PEIA [ EI ] := ORD ( EI ) ;
  for EI := SIX DOWNTO TWO do
    WRITE ( PEIA [ EI ] : 1 , ' ' ) ;
  WRITELN ( ' s/b  5 4 3 2 1' ) ;

  (***************************)
  (* multidementional arrays *)
  (***************************)

  WRITELN ( 'Array35:' ) ;
  Z := 0 ;
  for X := 1 to 10 do
    for Y := 1 to 10 do
      begin
        DA [ Y , X ] := Z ;
        Z := Z + 1
      end (* for *) ;
  for X := 1 to 10 do
    begin
      for Y := 1 to 10 do
        WRITE ( DA [ X ] [ Y ] : 2 , ' ' ) ;
      WRITELN ;
    end (* for *) ;
  WRITELN ( 's/b' ) ;
  WRITELN ( '0 10 20 30 40 50 60 70 80 90' ) ;
  WRITELN ( '1 11 21 31 41 51 61 71 81 91' ) ;
  WRITELN ( '2 12 22 32 42 52 62 72 82 92' ) ;
  WRITELN ( '3 13 23 33 43 53 63 73 83 93' ) ;
  WRITELN ( '4 14 24 34 44 54 64 74 84 94' ) ;
  WRITELN ( '5 15 25 35 45 55 65 75 85 95' ) ;
  WRITELN ( '6 16 26 36 46 56 66 76 86 96' ) ;
  WRITELN ( '7 17 27 37 47 57 67 77 87 97' ) ;
  WRITELN ( '8 18 28 38 48 58 68 78 88 98' ) ;
  WRITELN ( '9 19 29 39 49 59 69 79 89 99' ) ;
  WRITELN ( 'Array36: ' ) ;
  T := 0 ;
  for I := 1 to 2 do
    for X := 1 to 2 do
      for Y := 1 to 2 do
        for Z := 1 to 2 do
          for Q := 1 to 2 do
            for N := 1 to 2 do
              begin
                MDAR [ I ] [ X , Y , Z ] [ Q ] [ N ] := T ;
                T := T + 1
              end (* for *) ;
  for I := 2 DOWNTO 1 do
    for X := 2 DOWNTO 1 do
      for Y := 2 DOWNTO 1 do
        begin
          for Z := 2 DOWNTO 1 do
            for Q := 2 DOWNTO 1 do
              for N := 2 DOWNTO 1 do
                WRITE ( MDAR [ I , X ] [ Y , Z ] [ Q ] [ N ] : 2 , ' '
                        ) ;
          WRITELN ;
        end (* for *) ;
  WRITELN ( 's/b:' ) ;
  WRITELN ( '63 62 61 60 59 58 57 56' ) ;
  WRITELN ( '55 54 53 52 51 50 49 48' ) ;
  WRITELN ( '47 46 45 44 43 42 41 40' ) ;
  WRITELN ( '39 38 37 36 35 34 33 32' ) ;
  WRITELN ( '31 30 29 28 27 26 25 24' ) ;
  WRITELN ( '23 22 21 20 19 18 17 16' ) ;
  WRITELN ( '15 14 13 12 11 10  9  8' ) ;
  WRITELN ( ' 7  6  5  4  3  2  1  0' ) ;

  (***************)
  (* assignments *)
  (***************)

  WRITELN ( 'Array37: ' ) ;
  PAVC := 'hello, guy' ;
  WRITELN ( PAVC , ' s/b hello, guy' ) ;
  WRITELN ( 'Array38: ' ) ;
  for I := 1 to 10 do
    AVI [ I ] := I + 10 ;
  AVI2 := AVI ;
  for I := 10 DOWNTO 1 do
    WRITE ( AVI2 [ I ] : 1 , ' ' ) ;
  WRITELN ( 's/b 20 19 18 17 16 15 14 13 12 11' ) ;
  WRITELN ( 'Array39: ' ) ;
  T := 0 ;
  for I := 1 to 2 do
    for X := 1 to 2 do
      for Y := 1 to 2 do
        for Z := 1 to 2 do
          for Q := 1 to 2 do
            for N := 1 to 2 do
              begin
                MDAR [ I ] [ X , Y , Z ] [ Q ] [ N ] := T ;
                T := T + 1
              end (* for *) ;
  MDAR2 := MDAR ;
  for I := 2 DOWNTO 1 do
    for X := 2 DOWNTO 1 do
      for Y := 2 DOWNTO 1 do
        begin
          for Z := 2 DOWNTO 1 do
            for Q := 2 DOWNTO 1 do
              for N := 2 DOWNTO 1 do
                WRITE ( MDAR2 [ I , X ] [ Y , Z ] [ Q ] [ N ] : 2 , ' '
                        ) ;
          WRITELN ;
        end (* for *) ;
  WRITELN ( 's/b:' ) ;
  WRITELN ( '63 62 61 60 59 58 57 56' ) ;
  WRITELN ( '55 54 53 52 51 50 49 48' ) ;
  WRITELN ( '47 46 45 44 43 42 41 40' ) ;
  WRITELN ( '39 38 37 36 35 34 33 32' ) ;
  WRITELN ( '31 30 29 28 27 26 25 24' ) ;
  WRITELN ( '23 22 21 20 19 18 17 16' ) ;
  WRITELN ( '15 14 13 12 11 10  9  8' ) ;
  WRITELN ( ' 7  6  5  4  3  2  1  0' ) ;

  (***********************)
  (* transfer procedures *)
  (***********************)

  WRITELN ( 'Array40: ' ) ;
  for I := 1 to 10 do
    PAVI [ I ] := I + 10 ;
  UNPACK ( PAVI , AVI , 1 ) ;
  for I := 10 DOWNTO 1 do
    WRITE ( AVI [ I ] : 1 , ' ' ) ;
  WRITELN ( 's/b 20 19 18 17 16 15 14 13 12 11' ) ;
  WRITELN ( 'Array41: ' ) ;
  for I := 1 to 10 do
    AVI [ I ] := I + 20 ;
  PACK ( AVI , 1 , PAVI ) ;
  for I := 10 DOWNTO 1 do
    WRITE ( PAVI [ I ] : 1 , ' ' ) ;
  WRITELN ( 's/b 30 29 28 27 26 25 24 23 22 21' ) ;
  WRITELN ( 'Array42: ' ) ;
  for I := 1 to 10 do
    PAVI [ I ] := I + 30 ;
  UNPACK ( PAVI , CIA , 'g' ) ;
  for CI := 'p' DOWNTO 'g' do
    WRITE ( CIA [ CI ] : 1 , ' ' ) ;
  WRITELN ( 's/b 40 39 38 37 36 35 34 33 32 31' ) ;
  WRITELN ( 'Array43: ' ) ;
  X := 1 ;
  for CI := 'a' to 'z' do
    begin
      CIA [ CI ] := X ;
      X := X + 1
    end (* for *) ;
  PACK ( CIA , 'm' , PAVI ) ;
  for I := 10 DOWNTO 1 do
    WRITE ( PAVI [ I ] : 1 , ' ' ) ;
  WRITELN ( 's/b 22 21 20 19 18 17 16 15 14 13' ) ;

  (*****************************************************************)
  (*                                                               *)
  (*                         Records                               *)
  (*                                                               *)
  (*****************************************************************)

  WRITELN ;
  WRITELN (
          '******************* records ******************************'
            ) ;
  WRITELN ;

  (********************)
  (* types in records *)
  (********************)

  WRITELN ( 'Record1:   ' ) ;
  AREC . I := 64 ;
  AREC . B := FALSE ;
  AREC . C := 'j' ;
  AREC . E := TWO ;
  AREC . ES := FOUR ;
  AREC . S := 12 ;
  AREC . R := 4545.12E-32 ;
  AREC . ST := 'what ? who' ;
  for I := 1 to 10 do
    AREC . A [ I ] := I + 20 ;
  AREC . RC . A := 2324 ;
  AREC . RC . B := 'y' ;
  AREC . STC := [ 'b' .. 'e' , 'i' ] ;
  NEW ( AREC . P ) ;
  AREC . P -> := 8454 ;
  WRITELN ( AREC . I : 1 , ' ' , AREC . B : 5 , ' ' , AREC . C : 1 ,
            ' ' , ORD ( AREC . E ) : 1 , ' ' , ORD ( AREC . ES ) : 1 ,
            ' ' , AREC . S : 1 , ' ' , AREC . R : 15 , ' ' , AREC . ST
            ) ;
  for I := 1 to 10 do
    WRITE ( AREC . A [ I ] : 1 , ' ' ) ;
  WRITELN ;
  WRITELN ( AREC . RC . A : 1 , ' ' , AREC . RC . B : 1 ) ;
  for CI := 'a' to 'j' do
    if CI in AREC . STC then
      WRITE ( CI )
    else
      WRITE ( '_' ) ;
  WRITELN ;
  WRITELN ( AREC . P -> : 1 ) ;
  WRITELN ( 's/b:' ) ;
  WRITELN ( '64 false j 1 3 12  4.54512000e-29 what ? who' ) ;
  WRITELN ( '21 22 23 24 25 26 27 28 29 30' ) ;
  WRITELN ( '2324 y' ) ;
  WRITELN ( '_bcde___i_' ) ;
  WRITELN ( '8454' ) ;
  WRITELN ( 'Record2:   ' ) ;
  PAREC . I := 64 ;
  PAREC . B := FALSE ;
  PAREC . C := 'j' ;
  PAREC . E := TWO ;
  PAREC . ES := FOUR ;
  PAREC . S := 12 ;
  PAREC . R := 4545.12E-32 ;
  PAREC . ST := 'what ? who' ;
  for I := 1 to 10 do
    PAREC . A [ I ] := I + 20 ;
  PAREC . RC . A := 2324 ;
  PAREC . RC . B := 'y' ;
  PAREC . STC := [ 'b' .. 'e' , 'i' ] ;
  NEW ( PAREC . P ) ;
  PAREC . P -> := 8454 ;
  WRITELN ( PAREC . I : 1 , ' ' , PAREC . B : 5 , ' ' , PAREC . C : 1 ,
            ' ' , ORD ( PAREC . E ) : 1 , ' ' , ORD ( PAREC . ES ) : 1
            , ' ' , PAREC . S : 1 , ' ' , PAREC . R : 15 , ' ' , PAREC
            . ST ) ;
  for I := 1 to 10 do
    WRITE ( PAREC . A [ I ] : 1 , ' ' ) ;
  WRITELN ;
  WRITELN ( PAREC . RC . A : 1 , ' ' , PAREC . RC . B : 1 ) ;
  for CI := 'a' to 'j' do
    if CI in PAREC . STC then
      WRITE ( CI )
    else
      WRITE ( '_' ) ;
  WRITELN ;
  WRITELN ( PAREC . P -> : 1 ) ;
  WRITELN ( 's/b:' ) ;
  WRITELN ( '64 false j 1 3 12  4.54512000e-29 what ? who' ) ;
  WRITELN ( '21 22 23 24 25 26 27 28 29 30' ) ;
  WRITELN ( '2324 y' ) ;
  WRITELN ( '_bcde___i_' ) ;
  WRITELN ( '8454' ) ;

  (******************************************)
  (* types in variants, and border clipping *)
  (******************************************)

  WRITE ( 'Record3:   ' ) ;
  VRA . I := 873 ;
  VRA . VT := VTI ;
  VRA . A := 427 ;
  VRA . VDI := 235 ;
  WRITE ( VRA . I : 1 , ' ' , ORD ( VRA . VT ) : 1 , ' ' , VRA . VDI :
          1 , ' ' , VRA . A : 1 ) ;
  WRITELN ( ' s/b 873 0 235 427' ) ;
  WRITE ( 'Record4:   ' ) ;
  VRA . I := 873 ;
  VRA . VT := VTB ;
  VRA . B := 427 ;
  VRA . VDB := TRUE ;
  WRITE ( VRA . I : 1 , ' ' , ORD ( VRA . VT ) : 1 , ' ' , VRA . VDB :
          5 , ' ' , VRA . B : 1 ) ;
  WRITELN ( ' s/b 873 1  true 427' ) ;
  WRITE ( 'Record5:   ' ) ;
  VRA . I := 873 ;
  VRA . VT := VTC ;
  VRA . C := 427 ;
  VRA . VDC := 'f' ;
  WRITE ( VRA . I : 1 , ' ' , ORD ( VRA . VT ) : 1 , ' ' , VRA . VDC ,
          ' ' , VRA . C : 1 ) ;
  WRITELN ( ' s/b 873 2 f 427' ) ;
  WRITE ( 'Record6:   ' ) ;
  VRA . I := 873 ;
  VRA . VT := VTE ;
  VRA . D := 427 ;
  VRA . VDE := NINE ;
  WRITE ( VRA . I : 1 , ' ' , ORD ( VRA . VT ) : 1 , ' ' , ORD ( VRA .
          VDE ) : 1 , ' ' , VRA . D : 1 ) ;
  WRITELN ( ' s/b 873 3 8 427' ) ;
  WRITE ( 'Record7:   ' ) ;
  VRA . I := 873 ;
  VRA . VT := VTES ;
  VRA . E := 427 ;
  VRA . VDES := FOUR ;
  WRITE ( VRA . I : 1 , ' ' , ORD ( VRA . VT ) : 1 , ' ' , ORD ( VRA .
          VDES ) : 1 , ' ' , VRA . E : 1 ) ;
  WRITELN ( ' s/b 873 4 3 427' ) ;
  WRITE ( 'Record8:   ' ) ;
  VRA . I := 873 ;
  VRA . VT := VTS ;
  VRA . F := 427 ;
  VRA . VDS := 12 ;
  WRITE ( VRA . I : 1 , ' ' , ORD ( VRA . VT ) : 1 , ' ' , VRA . VDS :
          1 , ' ' , VRA . F : 1 ) ;
  WRITELN ( ' s/b 873 5 12 427' ) ;
  WRITE ( 'Record9:   ' ) ;
  VRA . I := 873 ;
  VRA . VT := VTR ;
  VRA . G := 427 ;
  VRA . VDR := 8734.8389 ;
  WRITE ( VRA . I : 1 , ' ' , ORD ( VRA . VT ) : 1 , ' ' , VRA . VDR :
          1 : 4 , ' ' , VRA . G : 1 ) ;
  WRITELN ( ' s/b 873 6 8734.8389 427' ) ;
  WRITE ( 'Record10:  ' ) ;
  VRA . I := 873 ;
  VRA . VT := VTST ;
  VRA . H := 427 ;
  VRA . VDST := 'this one ?' ;
  WRITE ( VRA . I : 1 , ' ' , ORD ( VRA . VT ) : 1 , ' ' , VRA . VDST ,
          ' ' , VRA . H : 1 ) ;
  WRITELN ( ' s/b 873 7 this one ? 427' ) ;
  WRITE ( 'Record11:  ' ) ;
  VRA . I := 873 ;
  VRA . VT := VTA ;
  VRA . J := 427 ;
  for I := 1 to 10 do
    VRA . VDA [ I ] := I + 10 ;
  WRITE ( VRA . I : 1 , ' ' , ORD ( VRA . VT ) : 1 , ' ' ) ;
  for I := 10 DOWNTO 1 do
    WRITE ( VRA . VDA [ I ] : 1 , ' ' ) ;
  WRITELN ( VRA . J : 1 ) ;
  WRITELN ( '      s/b:  873 8 20 19 18 17 16 15 14 13 12 11 427' ) ;
  WRITE ( 'Record12:  ' ) ;
  VRA . I := 873 ;
  VRA . VT := VTRC ;
  VRA . K := 427 ;
  VRA . VDRC . A := 2387 ;
  VRA . VDRC . B := 't' ;
  WRITE ( VRA . I : 1 , ' ' , ORD ( VRA . VT ) : 1 , ' ' , VRA . VDRC .
          A : 1 , ' ' , VRA . VDRC . B , ' ' , VRA . K : 1 ) ;
  WRITELN ( ' s/b:  873 9 2387 t 427' ) ;
  WRITE ( 'Record13:  ' ) ;
  VRA . I := 873 ;
  VRA . VT := VTSTC ;
  VRA . L := 427 ;
  VRA . VDSTC := [ 'b' .. 'g' , 'i' ] ;
  WRITE ( VRA . I : 1 , ' ' , ORD ( VRA . VT ) : 1 , ' ' ) ;
  for CI := 'j' DOWNTO 'a' do
    if CI in VRA . VDSTC then
      WRITE ( CI )
    else
      WRITE ( '_' ) ;
  WRITELN ( ' ' , VRA . L : 1 ) ;
  WRITELN ( '      s/b:  873 10 _i_gfedcb_ 427' ) ;
  WRITE ( 'Record14:  ' ) ;
  VRA . I := 873 ;
  VRA . VT := VTP ;
  VRA . M := 427 ;
  NEW ( VRA . VDP ) ;
  VRA . VDP -> := 2394 ;
  WRITE ( VRA . I : 1 , ' ' , ORD ( VRA . VT ) : 1 , ' ' , VRA . VDP ->
          : 1 , ' ' , VRA . M : 1 ) ;
  WRITELN ( ' s/b 873 11 2394 427' ) ;

  (*************************)
  (* types of variant tags *)
  (*************************)

  WRITE ( 'Record15:  ' ) ;
  VVRS . VT := 10 ;
  VVRS . VI := 2343 ;
  WRITE ( VVRS . VT : 1 , ' ' , VVRS . VI : 1 ) ;
  WRITELN ( ' s/b 10 2343' ) ;
  WRITE ( 'Record16:  ' ) ;
  VVRS . VT := 19 ;
  VVRS . VB := TRUE ;
  WRITE ( VVRS . VT : 1 , ' ' , VVRS . VB : 5 ) ;
  WRITELN ( ' s/b 19  true' ) ;
  WRITE ( 'Record17:  ' ) ;
  VVRB . VT := TRUE ;
  VVRB . VI := 2343 ;
  WRITE ( VVRB . VT : 5 , ' ' , VVRB . VI : 1 ) ;
  WRITELN ( ' s/b  true 2343' ) ;
  WRITE ( 'Record18:  ' ) ;
  VVRB . VT := FALSE ;
  VVRB . VB := TRUE ;
  WRITE ( VVRB . VT : 5 , ' ' , VVRB . VB : 5 ) ;
  WRITELN ( ' s/b false  true' ) ;
  WRITE ( 'Record19:  ' ) ;
  VVRE . VT := THREE ;
  VVRE . VI := 2343 ;
  WRITE ( ORD ( VVRE . VT ) : 1 , ' ' , VVRE . VI : 1 ) ;
  WRITELN ( ' s/b 2 2343' ) ;
  WRITE ( 'Record20:  ' ) ;
  VVRE . VT := EIGHT ;
  VVRE . VB := TRUE ;
  WRITE ( ORD ( VVRE . VT ) : 1 , ' ' , VVRE . VB : 5 ) ;
  WRITELN ( ' s/b 7  true' ) ;
  WRITE ( 'Record21:  ' ) ;
  VVRES . VT := FOUR ;
  VVRES . VI := 2343 ;
  WRITE ( ORD ( VVRES . VT ) : 1 , ' ' , VVRES . VI : 1 ) ;
  WRITELN ( ' s/b 3 2343' ) ;
  WRITE ( 'Record22:  ' ) ;
  VVRES . VT := FIVE ;
  VVRES . VB := TRUE ;
  WRITE ( ORD ( VVRES . VT ) : 1 , ' ' , VVRES . VB : 5 ) ;
  WRITELN ( ' s/b 4  true' ) ;

  (******************)
  (* nested records *)
  (******************)

  WRITE ( 'Record23:  ' ) ;
  NVR . I := 1 ;
  NVR . R . I := 2 ;
  NVR . R . R . I := 3 ;
  NVR . R . R . R . I := 4 ;
  NVR . R . R . R . R . I := 5 ;
  NVR . R . R . R . R . R . I := 6 ;
  NVR . R . R . R . R . R . R . I := 7 ;
  NVR . R . R . R . R . R . R . R . I := 8 ;
  NVR . R . R . R . R . R . R . R . R . I := 9 ;
  NVR . R . R . R . R . R . R . R . R . R . I := 10 ;
  WRITELN ( NVR . I : 1 , ' ' , NVR . R . I : 1 , ' ' , NVR . R . R . I
            : 1 , ' ' , NVR . R . R . R . I : 1 , ' ' , NVR . R . R . R
            . R . I : 1 , ' ' , NVR . R . R . R . R . R . I : 1 , ' ' ,
            NVR . R . R . R . R . R . R . I : 1 , ' ' , NVR . R . R . R
            . R . R . R . R . I : 1 , ' ' , NVR . R . R . R . R . R . R
            . R . R . I : 1 , ' ' , NVR . R . R . R . R . R . R . R . R
            . R . I : 1 , ' ' , 's/b 1 2 3 4 5 6 7 8 9 10' ) ;

  (*********************)
  (* 'with' statements *)
  (*********************)

  WRITE ( 'Record24:  ' ) ;
  with NVR do
    begin
      I := 10 ;
      with R do
        begin
          I := 9 ;
          with R do
            begin
              I := 8 ;
              with R do
                begin
                  I := 7 ;
                  with R do
                    begin
                      I := 6 ;
                      with R do
                        begin
                          I := 5 ;
                          with R do
                            begin
                              I := 4 ;
                              with R do
                                begin
                                  I := 3 ;
                                  with R do
                                    begin
                                      I := 2 ;
                                      with R do
                                        begin
                                          I := 2 ;
                                          with R do
                                            begin
                                              I := 1
                                            end (* with *)
                                        end (* with *)
                                    end (* with *)
                                end (* with *)
                            end (* with *)
                        end (* with *)
                    end (* with *)
                end (* with *)
            end (* with *)
        end (* with *)
    end (* with *) ;
  WRITELN ( NVR . I : 1 , ' ' , NVR . R . I : 1 , ' ' , NVR . R . R . I
            : 1 , ' ' , NVR . R . R . R . I : 1 , ' ' , NVR . R . R . R
            . R . I : 1 , ' ' , NVR . R . R . R . R . R . I : 1 , ' ' ,
            NVR . R . R . R . R . R . R . I : 1 , ' ' , NVR . R . R . R
            . R . R . R . R . I : 1 , ' ' , NVR . R . R . R . R . R . R
            . R . R . I : 1 , ' ' , NVR . R . R . R . R . R . R . R . R
            . R . I : 1 , ' ' , 's/b 10 9 8 7 6 5 4 3 2 1' ) ;
  WRITE ( 'Record25:  ' ) ;
  with NVR , R , R , R , R , R , R , R , R , R do
    I := 76 ;
  WRITELN ( NVR . I : 1 , ' ' , NVR . R . I : 1 , ' ' , NVR . R . R . I
            : 1 , ' ' , NVR . R . R . R . I : 1 , ' ' , NVR . R . R . R
            . R . I : 1 , ' ' , NVR . R . R . R . R . R . I : 1 , ' ' ,
            NVR . R . R . R . R . R . R . I : 1 , ' ' , NVR . R . R . R
            . R . R . R . R . I : 1 , ' ' , NVR . R . R . R . R . R . R
            . R . R . I : 1 , ' ' , NVR . R . R . R . R . R . R . R . R
            . R . I : 1 , ' ' , 's/b 10 9 8 7 6 5 4 3 2 76' ) ;
  WRITE ( 'Record26:  ' ) ;
  NEW ( RPA ) ;
  with RPA -> do
    begin
      I := 1 ;
      with RC do
        B := 'g'
    end (* with *) ;
  WRITELN ( RPA -> . I : 1 , ' ' , RPA -> . RC . B , ' s/b 1 g' ) ;
  WRITE ( 'Record27:  ' ) ;
  for I := 1 to 10 do
    with ARA [ I ] do
      A := I + 10 ;
  for I := 10 DOWNTO 1 do
    with ARA [ I ] do
      WRITE ( A : 1 , ' ' ) ;
  WRITELN ( 's/b 20 19 18 17 16 15 14 13 12 11' ) ;

  (*****************************************************************)
  (*                                                               *)
  (*                         Files                                 *)
  (*                                                               *)
  (*****************************************************************)

  if TESTFILE then
    begin
      WRITELN ;
      WRITELN (
            '******************* files ******************************'
                ) ;
      WRITELN ;

  (*******************)
  (* file base types *)
  (*******************)

      WRITE ( 'File1:   ' ) ;
      REWRITE ( FI ) ;
      for I := 1 to 10 do
        WRITE ( FI , I + 10 ) ;
      RESET ( FI ) ;
      for I := 1 to 10 do
        begin
          READ ( FI , X ) ;
          WRITE ( X : 1 , ' ' )
        end (* for *) ;
      WRITELN ( 's/b 11 12 13 14 15 16 17 18 19 20' ) ;
      WRITE ( 'File2:   ' ) ;
      REWRITE ( PFI ) ;
      for I := 1 to 10 do
        WRITE ( PFI , I + 10 ) ;
      RESET ( PFI ) ;
      for I := 1 to 10 do
        begin
          READ ( PFI , X ) ;
          WRITE ( X : 1 , ' ' )
        end (* for *) ;
      WRITELN ( 's/b 11 12 13 14 15 16 17 18 19 20' ) ;
      WRITE ( 'File3:   ' ) ;
      REWRITE ( FB ) ;
      for I := 1 to 10 do
        WRITE ( FB , ODD ( I ) ) ;
      RESET ( FB ) ;
      for I := 1 to 10 do
        begin
          READ ( FB , BA ) ;
          WRITE ( BA : 5 , ' ' )
        end (* for *) ;
      WRITELN ;
      WRITELN (
    '   s/b:    true false  true false  true false  true false  true '
                , 'false' ) ;
      WRITE ( 'File4:   ' ) ;
      REWRITE ( PFB ) ;
      for I := 1 to 10 do
        WRITE ( PFB , ODD ( I ) ) ;
      RESET ( PFB ) ;
      for I := 1 to 10 do
        begin
          READ ( PFB , BA ) ;
          WRITE ( BA : 5 , ' ' )
        end (* for *) ;
      WRITELN ;
      WRITELN (
    '   s/b:    true false  true false  true false  true false  true '
                , 'false' ) ;
      WRITE ( 'File5:   ' ) ;
      REWRITE ( FC ) ;
      for CI := 'a' to 'j' do
        WRITE ( FC , CI ) ;
      RESET ( FC ) ;
      for CI := 'a' to 'j' do
        begin
          READ ( FC , CA ) ;
          WRITE ( CA , ' ' )
        end (* for *) ;
      WRITELN ( 's/b a b c d e f g h i j' ) ;
      WRITE ( 'File6:   ' ) ;
      REWRITE ( PFC ) ;
      for CI := 'a' to 'j' do
        WRITE ( PFC , CI ) ;
      RESET ( PFC ) ;
      for CI := 'a' to 'j' do
        begin
          READ ( PFC , CA ) ;
          WRITE ( CA , ' ' )
        end (* for *) ;
      WRITELN ( 's/b a b c d e f g h i j' ) ;
      WRITE ( 'File7:   ' ) ;
      REWRITE ( FE ) ;
      for EI := ONE to TEN do
        WRITE ( FE , EI ) ;
      RESET ( FE ) ;
      for EI := ONE to TEN do
        begin
          READ ( FE , EA ) ;
          WRITE ( ORD ( EA ) : 1 , ' ' )
        end (* for *) ;
      WRITELN ( 's/b 0 1 2 3 4 5 6 7 8 9' ) ;
      WRITE ( 'File8:   ' ) ;
      REWRITE ( PFE ) ;
      for EI := ONE to TEN do
        WRITE ( PFE , EI ) ;
      RESET ( PFE ) ;
      for EI := ONE to TEN do
        begin
          READ ( PFE , EA ) ;
          WRITE ( ORD ( EA ) : 1 , ' ' )
        end (* for *) ;
      WRITELN ( 's/b 0 1 2 3 4 5 6 7 8 9' ) ;

  (*************************)
  (* types written to text *)
  (*************************)

      WRITELN ( 'File9:' ) ;
      REWRITE ( FT ) ;
      X := 7384 ;
      WRITELN ( FT , X : 1 ) ;
      WRITELN ( FT , 8342 : 1 ) ;
      BA := TRUE ;
      WRITELN ( FT , BA : 5 ) ;
      WRITELN ( FT , FALSE : 5 ) ;
      CA := 'm' ;
      WRITELN ( FT , CA ) ;
      WRITELN ( FT , 'q' ) ;
      RA := 1234.5678E-3 ;
      WRITELN ( FT , RA : 15 ) ;
      WRITELN ( FT , RA : 1 : 7 ) ;
      WRITELN ( FT , 5689.4321E-2 : 15 ) ;
      WRITELN ( FT , 9383.7632E-4 : 1 : 8 ) ;
      S := 'hi there !' ;
      WRITELN ( FT , S ) ;
      WRITELN ( FT , S : 5 ) ;
      WRITELN ( FT , S : 15 ) ;
      RESET ( FT ) ;
      while not EOF ( FT ) do
        begin
          if EOLN ( FT ) then
            begin
              READLN ( FT ) ;
              WRITELN
            end (* then *)
          else
            begin
              READ ( FT , CI ) ;
              WRITE ( CI )
            end (* else *)
        end (* while *) ;
      WRITELN ( 's/b:' ) ;
      WRITELN ( '7384' ) ;
      WRITELN ( '8342' ) ;
      WRITELN ( ' true' ) ;
      WRITELN ( 'false' ) ;
      WRITELN ( 'm' ) ;
      WRITELN ( 'q' ) ;
      WRITELN ( ' 1.2345678000e+00' ) ;
      WRITELN ( '1.2345678' ) ;
      WRITELN ( ' 5.6894321000e+01' ) ;
      WRITELN ( '0.93837632' ) ;
      WRITELN ( 'hi there !' ) ;
      WRITELN ( 'hi th' ) ;
      WRITELN ( '     hi there !' ) ;

  (************************)
  (* types read from text *)
  (************************)

      WRITELN ( 'file10:' ) ;
      RESET ( FT ) ;
      READLN ( FT , Y ) ;
      WRITELN ( Y : 1 ) ;
      READLN ( FT , Y ) ;
      WRITELN ( Y : 1 ) ;
      READLN ( FT ) ;
      READLN ( FT ) ;
      READLN ( FT , CI ) ;
      WRITELN ( CI ) ;
      READLN ( FT , CI ) ;
      WRITELN ( CI ) ;
      READLN ( FT , RB ) ;
      WRITELN ( RB : 15 ) ;
      READLN ( FT , RB ) ;
      WRITELN ( RB : 15 ) ;
      READLN ( FT , RB ) ;
      WRITELN ( RB : 15 ) ;
      READLN ( FT , RB ) ;
      WRITELN ( RB : 15 ) ;
      WRITELN ( 's/b:' ) ;
      WRITELN ( '7384' ) ;
      WRITELN ( '8342' ) ;
      WRITELN ( 'm' ) ;
      WRITELN ( 'q' ) ;
      WRITELN ( ' 1.2345678000e+00' ) ;
      WRITELN ( ' 1.2345678000e+00' ) ;
      WRITELN ( ' 5.6894321000e+01' ) ;
      WRITELN ( ' 9.3837632000e+01' ) ;

  (*********************************)
  (* line and file endings in text *)
  (*********************************)

      WRITELN ( 'file11:' ) ;
      REWRITE ( FT ) ;
      WRITELN ( FT , 'how now' ) ;
      WRITELN ( FT , 'brown cow' ) ;
      RESET ( FT ) ;
      WRITE ( '''' ) ;
      while not EOF ( FT ) do
        begin
          if EOLN ( FT ) then
            WRITE ( '<eoln>' ) ;
          READ ( FT , CA ) ;
          WRITE ( CA )
        end (* while *) ;
      WRITE ( '''' ) ;
      WRITELN ( ' s/b ''how now<eoln> brown cow<eoln> ''' ) ;
      WRITELN ( 'file12:' ) ;
      REWRITE ( FT ) ;
      WRITELN ( FT , 'too much' ) ;
      WRITE ( FT , 'too soon' ) ;
      RESET ( FT ) ;
      WRITE ( '''' ) ;
      while not EOF ( FT ) do
        begin
          if EOLN ( FT ) then
            WRITE ( '<eoln>' ) ;
          READ ( FT , CA ) ;
          WRITE ( CA )
        end (* while *) ;
      WRITE ( '''' ) ;
      WRITELN ( ' s/b ''too much<eoln> too soon<eoln> ''' ) ;

  (********************************)
  (* get/put and buffer variables *)
  (********************************)

      WRITE ( 'File13:   ' ) ;
      REWRITE ( FI ) ;
      for I := 1 to 10 do
        begin
          FI -> := I + 10 ;
          PUT ( FI )
        end (* for *) ;
      RESET ( FI ) ;
      for I := 1 to 10 do
        begin
          X := FI -> ;
          GET ( FI ) ;
          WRITE ( X : 1 , ' ' )
        end (* for *) ;
      WRITELN ( 's/b 11 12 13 14 15 16 17 18 19 20' ) ;
      WRITE ( 'File14:   ' ) ;
      REWRITE ( PFI ) ;
      for I := 1 to 10 do
        begin
          PFI -> := I + 10 ;
          PUT ( PFI )
        end (* for *) ;
      RESET ( PFI ) ;
      for I := 1 to 10 do
        begin
          X := PFI -> ;
          GET ( PFI ) ;
          WRITE ( X : 1 , ' ' )
        end (* for *) ;
      WRITELN ( 's/b 11 12 13 14 15 16 17 18 19 20' ) ;
      WRITE ( 'File15:   ' ) ;
      REWRITE ( FB ) ;
      for I := 1 to 10 do
        begin
          FB -> := ODD ( I ) ;
          PUT ( FB )
        end (* for *) ;
      RESET ( FB ) ;
      for I := 1 to 10 do
        begin
          BA := FB -> ;
          GET ( FB ) ;
          WRITE ( BA : 5 , ' ' )
        end (* for *) ;
      WRITELN ;
      WRITELN (
    '   s/b:    true false  true false  true false  true false  true '
                , 'false' ) ;
      WRITE ( 'File16:   ' ) ;
      REWRITE ( PFB ) ;
      for I := 1 to 10 do
        begin
          PFB -> := ODD ( I ) ;
          PUT ( PFB )
        end (* for *) ;
      RESET ( PFB ) ;
      for I := 1 to 10 do
        begin
          BA := PFB -> ;
          GET ( PFB ) ;
          WRITE ( BA : 5 , ' ' )
        end (* for *) ;
      WRITELN ;
      WRITELN (
    '   s/b:    true false  true false  true false  true false  true '
                , 'false' ) ;
      WRITE ( 'File17:   ' ) ;
      REWRITE ( FC ) ;
      for CI := 'a' to 'j' do
        begin
          FC -> := CI ;
          PUT ( FC )
        end (* for *) ;
      RESET ( FC ) ;
      for CI := 'a' to 'j' do
        begin
          CA := FC -> ;
          GET ( FC ) ;
          WRITE ( CA , ' ' )
        end (* for *) ;
      WRITELN ( 's/b a b c d e f g h i j' ) ;
      WRITE ( 'File18:   ' ) ;
      REWRITE ( PFC ) ;
      for CI := 'a' to 'j' do
        begin
          PFC -> := CI ;
          PUT ( PFC )
        end (* for *) ;
      RESET ( PFC ) ;
      for CI := 'a' to 'j' do
        begin
          CA := PFC -> ;
          GET ( PFC ) ;
          WRITE ( CA , ' ' )
        end (* for *) ;
      WRITELN ( 's/b a b c d e f g h i j' ) ;
      WRITE ( 'File19:   ' ) ;
      REWRITE ( FE ) ;
      for EI := ONE to TEN do
        begin
          FE -> := EI ;
          PUT ( FE )
        end (* for *) ;
      RESET ( FE ) ;
      for EI := ONE to TEN do
        begin
          EA := FE -> ;
          GET ( FE ) ;
          WRITE ( ORD ( EA ) : 1 , ' ' )
        end (* for *) ;
      WRITELN ( 's/b 0 1 2 3 4 5 6 7 8 9' ) ;
      WRITE ( 'File20:   ' ) ;
      REWRITE ( PFE ) ;
      for EI := ONE to TEN do
        begin
          PFE -> := EI ;
          PUT ( PFE )
        end (* for *) ;
      RESET ( PFE ) ;
      for EI := ONE to TEN do
        begin
          EA := PFE -> ;
          GET ( PFE ) ;
          WRITE ( ORD ( EA ) : 1 , ' ' )
        end (* for *) ;
      WRITELN ( 's/b 0 1 2 3 4 5 6 7 8 9' ) ;
      WRITE ( 'File21:   ' ) ;
      REWRITE ( FT ) ;
      WRITELN ( FT , '50' ) ;
      RESET ( FT ) ;
      READ ( FT , SRX ) ;
      WRITELN ( ' s/b ' , 50 : 1 ) ;
      WRITE ( 'File22:   ' ) ;
      REWRITE ( FT ) ;
      WRITELN ( EOF ( FT ) , ' s/b true' ) ;
    end (* then *) ;

  (*****************************************************************)
  (*                                                               *)
  (*                      Procedures and functions                 *)
  (*                                                               *)
  (*****************************************************************)

  WRITELN ;
  WRITELN ( '************ Procedures and functions ******************'
            ) ;
  WRITELN ;
  WRITE ( 'ProcedureFunction1:   ' ) ;
  X := 45 ;
  Y := 89 ;
  JUNK1 ( X , Y ) ;
  WRITELN ( ' s/b 45 89' ) ;
  WRITE ( 'ProcedureFunction2:   ' ) ;
  X := 45 ;
  JUNK2 ( X ) ;
  WRITELN ( X : 1 , ' s/b 46' ) ;
  WRITE ( 'ProcedureFunction3:   ' ) ;
  S := 'total junk' ;
  JUNK3 ( S ) ;
  WRITELN ( ' s/b total junk' ) ;
  WRITE ( 'ProcedureFunction4:   ' ) ;
  S := 'total junk' ;
  JUNK4 ( S ) ;
  WRITELN ( ' s/b tota? junk' ) ;
  WRITELN ( S , ' s/b total junk' ) ;
  WRITE ( 'ProcedureFunction5:   ' ) ;
  WRITELN ( JUNK5 ( 34 ) : 1 , ' s/b 35' ) ;
  WRITE ( 'ProcedureFunction6:   ' ) ;
  I := JUNK7 ( 10 , 9 , 8 ) ;
  WRITELN ( ' ' , I : 1 ) ;
  WRITELN ( 's/b:   10 9 8 6 5 4 3 2 1 78' ) ;
  WRITELN ( 'ProcedureFunction7:' ) ;
  for I := 1 to 10 do
    AI [ I ] := I + 10 ;
  AREC . I := 64 ;
  AREC . B := FALSE ;
  AREC . C := 'j' ;
  AREC . E := TWO ;
  AREC . ES := FOUR ;
  AREC . S := 12 ;
  AREC . R := 4545.12E-32 ;
  AREC . ST := 'what ? who' ;
  for I := 1 to 10 do
    AREC . A [ I ] := I + 20 ;
  AREC . RC . A := 2324 ;
  AREC . RC . B := 'y' ;
  AREC . STC := [ 'b' .. 'e' , 'i' ] ;
  NEW ( AREC . P ) ;
  AREC . P -> := 8454 ;
  VREC . A := 23487 ;
  VREC . B := 'n' ;
  VREC . C := FALSE ;
  VREC . D := 'help me123' ;
  NEW ( IP ) ;
  IP -> := 734 ;
  JUNK8 ( 93 , TRUE , 'k' , EIGHT , FIVE , 10 , 3.1414 , 'hello, guy' ,
          AI , AREC , VREC , [ 'a' .. 'd' , 'h' ] , IP ) ;
  WRITELN ( 's/b:' ) ;
  WRITELN ( '93  true k 7 4 10  3.14140000e+00 hello, guy' ) ;
  WRITELN ( '11 12 13 14 15 16 17 18 19 20' ) ;
  WRITELN ( '64 false j 1 3 12  4.54500000e-29 what ? who' ) ;
  WRITELN ( '21 22 23 24 25 26 27 28 29 30' ) ;
  WRITELN ( '2324 y' ) ;
  WRITELN ( '_bcde___i_' ) ;
  WRITELN ( '8454' ) ;
  WRITELN ( '23487 n false' ) ;
  WRITELN ( 'help me123' ) ;
  WRITELN ( 'abcd___h__' ) ;
  WRITELN ( '734' ) ;
  WRITE ( 'ProcedureFunction8:   ' ) ;
  JUNK9 ( JUNK10 , JUNK11 ) ;
  WRITELN ( ' s/b 9834 8383 j 744' ) ;
  WRITE ( 'ProcedureFunction9:   ' ) ;
  JUNK12 ( JUNK13 , JUNK11 ) ;
  WRITELN ( ' s/b 942' ) ;
  WRITE ( 'ProcedureFunction10:   ' ) ;
  JUNK14 ;
  WRITELN ( ' s/b 62 76' ) ;
  WRITE ( 'ProcedureFunction11:   ' ) ;
  JUNK17 ( JUNK16 , 52 ) ;
  WRITELN ( ' s/b 52' ) ;
  WRITE ( 'ProcedureFunction12:   ' ) ;
  JUNK19 ;
  WRITELN ( ' s/b a' ) ;
  WRITE ( 'ProcedureFunction13:   ' ) ;
  WRITELN ( JUNK20 : 1 , ' s/b 37' ) ;
end (* HAUPTPROGRAMM *) .
