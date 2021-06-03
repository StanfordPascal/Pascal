module $PASLIBX ;

(********************************************************************)
(*$D+,A+                                                            *)
(********************************************************************)
(*                                                                  *)
(*  Stanford Pascal runtime / Pascal parts                          *)
(*                                                                  *)
(*  created 10.2016 - bernd.oppolzer@yahoo.com                      *)
(*                                                                  *)
(*  contains                                                        *)
(*                                                                  *)
(*  - new storage management ALLOC and FREE (inspired by LE)        *)
(*                                                                  *)
(*  - (advanced) rounding function for double floats                *)
(*                                                                  *)
(*  - string handling functions (from Pascal/VS)                    *)
(*                                                                  *)
(*  - new READ functions for integers and reals                     *)
(*                                                                  *)
(*  - other fancy stuff                                             *)
(*                                                                  *)
(*  only top level functions and procedures which don't have        *)
(*  the local specification are seen by the linkage editor          *)
(*                                                                  *)
(*  caution: the functions and procedures beginning with $PAS       *)
(*  are normally used by the compiler (that is, the compiler        *)
(*  generates calls to them in the open code), they should          *)
(*  only be changed with care                                       *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  Writing parts of the runtime in Pascal could seem critical,     *)
(*  especially because the language is still interpreted on         *)
(*  non-Mainframe platforms. But anyway I decided often to go       *)
(*  this path, because this way the solution is portable from       *)
(*  the start, and it is much easier to apply changes (only in      *)
(*  one place). If there are performance problems (which is         *)
(*  not the case at the moment), I would try to generate            *)
(*  real machine code even on non-Mainframe platforms               *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  uses static variables - see static definitions                  *)
(*                                                                  *)
(********************************************************************)
(*                                                                  *)
(*  History:                                                        *)
(*                                                                  *)
(*  16.08.2020: new function $PASRDB - read boolean                 *)
(*                                                                  *)
(*  12.01.2020: new function $PASRDR - read reals                   *)
(*                                                                  *)
(*  11.01.2020: all former EXIT calls replaced by $ERROR calls      *)
(*  $ERROR calls will trigger error recovery from runtime           *)
(*  for example stack trace etc. (symbolic dump on the mainframe    *)
(*  if compiled with the DEBUG option, which is the default)        *)
(*                                                                  *)
(*  10.01.2020: new function $PASRDI - read integers                *)
(*  replaces platform specific implementations of CSPs RDI, RDH     *)
(*  and RDY (read integers of length 4, 2 and 1).                   *)
(*  Compiler uses $PASRDI only, old implementations will be         *)
(*  phased out ...                                                  *)
(*                                                                  *)
(*  17.02.2019: found critical error in MODIFY_TREE                 *)
(*  near the call of SUCHE_HFRE (look at the variable               *)
(*  VMODUS2 and inserting the new HFRE when same length)            *)
(*                                                                  *)
(********************************************************************)



const HANCSIZE = 65536 ;
      TRLEVEL = 0 ;
      BUCHST : set of CHAR =
      [ 'A' .. 'I' , 'J' .. 'R' , 'S' .. 'Z' , 'a' .. 'i' , 'j' .. 'r'
        , 's' .. 'z' ] ;
      KLEINBUCHST : set of CHAR =
      [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' ] ;
      IDCHARS : set of CHAR =
      [ 'A' .. 'I' , 'J' .. 'R' , 'S' .. 'Z' , 'a' .. 'i' , 'j' .. 'r'
        , 's' .. 'z' , '0' .. '9' , '_' , '$' ] ;


type CHARPTR = -> CHAR ;
     PLATFORM = ( PLATF_UNKNOWN , PLATF_INTEL , PLATF_MAINFRAME ) ;
     CHAR128 = array [ 1 .. 128 ] of CHAR ;
     TOKEN = array [ 1 .. 8 ] of CHAR ;
     CHAR4 = array [ 1 .. 4 ] of CHAR ;

     (****************************************************)
     (*  HFRE: Free Element innerhalb HANC               *)
     (****************************************************)

     PHFRE = -> HFRE ;
     HFRE = record
              FREELOW : PHFRE ;
              FREEEQ : PHFRE ;
              LEN_FREELOW : INTEGER ;
              LEN_FREEEQ : INTEGER ;
            end ;

     (****************************************************)
     (*  HANC: Heap Element                              *)
     (****************************************************)

     PHANC = -> HANC ;
     HANC = record
              EYECATCH : CHAR4 ;
              FWD : PHANC ;
              BWD : PHANC ;
              DUMMY : INTEGER ;
              AREA1 : VOIDPTR ;
              FREE1 : PHFRE ;
              LEN_AREA1 : INTEGER ;
              LEN_FREE1 : INTEGER ;
            end ;

     (****************************************************)
     (*  Active Element                                  *)
     (****************************************************)

     PACT = -> ACTIVE ;
     ACTIVE = record
                XHANC : PHANC ;
                XSIZE : INTEGER ;
              end ;

     (****************************************************)
     (*  HPCB: Heap Control Block                        *)
     (****************************************************)

     PHPCB = -> HPCB ;
     HPCB = record
              EYECATCH : CHAR4 ;
              FIRST : PHANC ;
              LAST : PHANC ;
              ACTIVE : INTEGER ;
            end ;


static PLATF : PLATFORM ;
       HEAPCB : HPCB ;
       PHEAP : PHPCB ;

       (****************************************************)
       (*  statische Variablen:                            *)
       (*  heap control block                              *)
       (*  zeiger auf heap control block (anfangs nil)     *)
       (*  anzahl-felder fuer allocs und frees             *)
       (****************************************************)

       ANZ_ALLOCS : INTEGER ;
       ANZ_FREES : INTEGER ;



function $PASSYS ( FUNCCODE : INTEGER ; X : VOIDPTR ) : VOIDPTR ;

(****************************************************)
(*  ist in PASMONN.ASS implementiert und            *)
(*  realisiert Basis-Storage-Dienste wie            *)
(*  GETMAIN und FREEMAIN                            *)
(****************************************************)


   EXTERNAL ;



local procedure CHECK_PLATFORM ;

   begin (* CHECK_PLATFORM *)
     if ORD ( '0' ) = 0X30 then
       PLATF := PLATF_INTEL
     else
       PLATF := PLATF_MAINFRAME
   end (* CHECK_PLATFORM *) ;



local function ALLOC_AREAX ( SIZE : INTEGER ) : VOIDPTR ;

(****************************************************************)
(*  works much the same as C malloc                             *)
(*  calls the OPSYS service (aka GETMAIN) directly              *)
(****************************************************************)


   type INT2PTR = record
                    case INTEGER of
                      0 :
                        ( P : VOIDPTR ) ;
                      1 :
                        ( I : INTEGER ) ;
                  end ;

   var IP : INT2PTR ;

   begin (* ALLOC_AREAX *)
     IP . I := SIZE ;
     ALLOC_AREAX := $PASSYS ( 1 , IP . P ) ;
   end (* ALLOC_AREAX *) ;



local procedure DUMP ( PVON : VOIDPTR ; PBIS : VOIDPTR ) ;

(*********************************************************)
(*  Speicherbereich von PVON bis PBIS hexadezimal        *)
(*  ausgeben                                             *)
(*********************************************************)


   var P1 : VOIDPTR ;
       P2 : VOIDPTR ;
       MOD1 : INTEGER ;
       MOD2 : INTEGER ;


   procedure DUMPCHAR ( CH : CHAR ) ;

      const DUMPSHOWCHARS : set of CHAR =
            [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' , 'A' .. 'I' , 'J'
              .. 'R' , 'S' .. 'Z' , '0' .. '9' , ' ' , ',' , '.' , '-'
              , ';' , ':' , '_' , '!' , '"' , '$' , '%' , '&' , '/' ,
              '(' , ')' , '=' , '?' , '+' , '*' , '#' , '*' ] ;

      begin (* DUMPCHAR *)
        if CH in DUMPSHOWCHARS then
          WRITE ( CH )
        else
          WRITE ( '.' )
      end (* DUMPCHAR *) ;


   procedure DUMPZEILE ( ADR : VOIDPTR ; P1 : VOIDPTR ; P2 : VOIDPTR )
                       ;

      var CH : -> CHAR ;
          I : INTEGER ;

      const HEXTAB : array [ 0 .. 15 ] of CHAR = '0123456789abcdef' ;

      begin (* DUMPZEILE *)
        WRITE ( ADR , ': ' ) ;
        CH := ADR ;
        if ( PTRDIFF ( ADR , P1 ) < 0 ) or ( PTRDIFF ( ADR , P2 ) > 0 )
        then
          WRITE ( '........ ' )
        else
          begin
            for I := 1 to 4 do
              begin
                WRITE ( HEXTAB [ ORD ( CH -> ) DIV 16 ] , HEXTAB [ ORD
                        ( CH -> ) MOD 16 ] ) ;
                CH := PTRADD ( CH , 1 ) ;
              end (* for *) ;
            WRITE ( ' ' ) ;
          end (* else *) ;
        ADR := PTRADD ( ADR , 4 ) ;
        CH := ADR ;
        if ( PTRDIFF ( ADR , P1 ) < 0 ) or ( PTRDIFF ( ADR , P2 ) > 0 )
        then
          WRITE ( '........ ' )
        else
          begin
            for I := 1 to 4 do
              begin
                WRITE ( HEXTAB [ ORD ( CH -> ) DIV 16 ] , HEXTAB [ ORD
                        ( CH -> ) MOD 16 ] ) ;
                CH := PTRADD ( CH , 1 ) ;
              end (* for *) ;
            WRITE ( ' ' ) ;
          end (* else *) ;
        ADR := PTRADD ( ADR , 4 ) ;
        CH := ADR ;
        if ( PTRDIFF ( ADR , P1 ) < 0 ) or ( PTRDIFF ( ADR , P2 ) > 0 )
        then
          WRITE ( '........ ' )
        else
          begin
            for I := 1 to 4 do
              begin
                WRITE ( HEXTAB [ ORD ( CH -> ) DIV 16 ] , HEXTAB [ ORD
                        ( CH -> ) MOD 16 ] ) ;
                CH := PTRADD ( CH , 1 ) ;
              end (* for *) ;
            WRITE ( ' ' ) ;
          end (* else *) ;
        ADR := PTRADD ( ADR , 4 ) ;
        CH := ADR ;
        if ( PTRDIFF ( ADR , P1 ) < 0 ) or ( PTRDIFF ( ADR , P2 ) > 0 )
        then
          WRITE ( '........ ' )
        else
          begin
            for I := 1 to 4 do
              begin
                WRITE ( HEXTAB [ ORD ( CH -> ) DIV 16 ] , HEXTAB [ ORD
                        ( CH -> ) MOD 16 ] ) ;
                CH := PTRADD ( CH , 1 ) ;
              end (* for *) ;
            WRITE ( ' ' ) ;
          end (* else *) ;
        ADR := PTRADD ( ADR , - 12 ) ;
        CH := ADR ;
        WRITE ( ' *' ) ;
        for I := 1 to 16 do
          begin
            DUMPCHAR ( CH -> ) ;
            CH := PTRADD ( CH , 1 )
          end (* for *) ;
        WRITELN ( '*' ) ;
      end (* DUMPZEILE *) ;


   begin (* DUMP *)
     WRITELN ( 'Dump Speicherbereich von ' , PVON , ' bis ' , PBIS ) ;
     P1 := PTRADD ( PVON , - 16 ) ;
     MOD1 := PTR2INT ( P1 ) MOD 16 ;
     P1 := PTRADD ( P1 , 16 - MOD1 ) ;
     P2 := PTRADD ( PBIS , 15 ) ;
     MOD2 := PTR2INT ( P2 ) MOD 16 ;
     P2 := PTRADD ( P2 , - MOD2 ) ;
     while PTRDIFF ( P1 , P2 ) < 0 do
       begin
         DUMPZEILE ( P1 , PVON , PBIS ) ;
         P1 := PTRADD ( P1 , 16 ) ;
       end (* while *) ;
   end (* DUMP *) ;



local procedure LISTE_DER_LUECKEN ( HANC : PHANC ) ;

(*********************************************************)
(*  diese Funktion zeigt alle Luecken an, die im         *)
(*  HANC-Baum organisiert sind                           *)
(*********************************************************)


   var PLAUF : PHFRE ;
       PEQUAL : PHFRE ;
       SIZE : INTEGER ;
       CHKENN : CHAR ;
       SIZEALT : INTEGER ;
       PEQUALALT : PHFRE ;

   begin (* LISTE_DER_LUECKEN *)
     WRITELN ( '*** Luecken im HANC ' , HANC , ' ***' ) ;
     PLAUF := HANC -> . FREE1 ;
     SIZE := HANC -> . LEN_FREE1 ;
     SIZEALT := SIZE ;
     while PLAUF <> NIL do
       begin
         CHKENN := '*' ;
         PEQUAL := PLAUF ;
         PEQUALALT := PTRADD ( PEQUAL , - 4 ) ;
         while PEQUAL <> NIL do
           with PEQUAL -> do
             begin
               if SIZE <> SIZEALT then
                 $ERROR ( 1102 ) ;
               if PTRDIFF ( PEQUAL , PEQUALALT ) <= 0 then
                 $ERROR ( 1103 ) ;
               PEQUALALT := PEQUAL ;
               WRITELN ( CHKENN , ' ' , 'Luecke bei ' , PEQUAL ,
                         ' Groesse ' , SIZE ) ;
               if SIZE > 8 then
                 SIZE := PEQUAL -> . LEN_FREEEQ ;
               CHKENN := '-' ;
               PEQUAL := FREEEQ ;
             end (* with *) ;
         if SIZEALT > 8 then
           begin
             SIZE := PLAUF -> . LEN_FREELOW ;
             if SIZE >= SIZEALT then
               $ERROR ( 1101 ) ;
           end (* then *)
         else
           begin
             if PLAUF -> . FREELOW <> NIL then
               $ERROR ( 1101 ) ;
             SIZE := 0 ;
           end (* else *) ;
         SIZEALT := SIZE ;
         PLAUF := PLAUF -> . FREELOW ;
       end (* while *) ;
     WRITELN ( 'Ende der Liste - Groesse ' , SIZE ) ;
   end (* LISTE_DER_LUECKEN *) ;



local procedure LISTE_ALLER_HANCS ( PHEAPC : PHPCB ; LUECKEN : BOOLEAN
                                  ) ;

   var HANC_CHK : PHANC ;
       FEHLER : BOOLEAN ;

   begin (* LISTE_ALLER_HANCS *)
     FEHLER := FALSE ;
     WRITELN ( 'liste_hancs: heapc = ' , PHEAPC ) ;
     if PHEAPC = NIL then
       begin
         WRITELN ( 'liste_hancs: heapc is nil, no heap access so far' )
                   ;
         return ;
       end (* then *) ;
     WRITELN ( 'liste_hancs: heapc.eyecatch = ' , PHEAPC -> . EYECATCH
               ) ;
     WRITELN ( 'liste_hancs: heapc.first = ' , PHEAPC -> . FIRST ) ;
     HANC_CHK := PHEAPC -> . FIRST ;
     if HANC_CHK = NIL then
       WRITELN ( 'liste_hancs: heap is empty' )
     else
       while HANC_CHK <> PTRCAST ( PHEAPC ) do
         begin
           WRITELN ( 'liste_hancs: hanc = ' , HANC_CHK , ' free1 = ' ,
                     HANC_CHK -> . FREE1 , ' size_free1 = ' , HANC_CHK
                     -> . LEN_FREE1 ) ;
           if HANC_CHK -> . LEN_FREE1 = 0 then
             begin
               if HANC_CHK -> . FREE1 <> NIL then
                 FEHLER := TRUE
             end (* then *)
           else
             if HANC_CHK -> . LEN_FREE1 > HANC_CHK -> . LEN_AREA1 then
               FEHLER := TRUE
             else
               if PTRDIFF ( HANC_CHK -> . FREE1 , HANC_CHK -> . AREA1 )
               < 0 then
                 FEHLER := TRUE
               else
                 if PTRDIFF ( PTRADD ( HANC_CHK -> . FREE1 , HANC_CHK
                 -> . LEN_FREE1 ) , PTRADD ( HANC_CHK -> . AREA1 ,
                 HANC_CHK -> . LEN_AREA1 ) ) > 0 then
                   FEHLER := TRUE ;
           if LUECKEN then
             begin
               LISTE_DER_LUECKEN ( HANC_CHK ) ;
             end (* then *) ;
           HANC_CHK := HANC_CHK -> . FWD
         end (* while *) ;
     if FEHLER then
       $ERROR ( 1105 ) ;
   end (* LISTE_ALLER_HANCS *) ;



local procedure REMOVE_HFRE ( V : CHAR ; P : PHFRE ; P_VOR : PHFRE ;
                            HANC_ACT : PHANC ; SIZE : INTEGER ) ;

   begin (* REMOVE_HFRE *)
     case V of
       'T' : begin
               if P -> . FREEEQ = NIL then
                 begin
                   HANC_ACT -> . FREE1 := P -> . FREELOW ;
                   if SIZE > 8 then
                     HANC_ACT -> . LEN_FREE1 := P -> . LEN_FREELOW
                   else
                     if P -> . FREELOW = NIL then
                       HANC_ACT -> . LEN_FREE1 := 0
                     else
                       HANC_ACT -> . LEN_FREE1 := 8
                 end (* then *)
               else
                 begin
                   P -> . FREEEQ -> . FREELOW := P -> . FREELOW ;
                   if SIZE > 8 then
                     P -> . FREEEQ -> . LEN_FREELOW := P -> .
                                                   LEN_FREELOW ;
                   HANC_ACT -> . FREE1 := P -> . FREEEQ ;
                   if SIZE > 8 then
                     HANC_ACT -> . LEN_FREE1 := P -> . LEN_FREEEQ
                   else
                     if P -> . FREEEQ = NIL then
                       HANC_ACT -> . LEN_FREE1 := 0
                     else
                       HANC_ACT -> . LEN_FREE1 := 8
                 end (* else *) ;
             end (* tag/ca *) ;
       'H' : begin
               P_VOR -> . FREEEQ := P -> . FREEEQ ;
               if SIZE > 8 then
                 P_VOR -> . LEN_FREEEQ := P -> . LEN_FREEEQ ;
             end (* tag/ca *) ;
       'V' : begin
               if P -> . FREEEQ = NIL then
                 begin
                   P_VOR -> . FREELOW := P -> . FREELOW ;
                   if SIZE > 8 then
                     P_VOR -> . LEN_FREELOW := P -> . LEN_FREELOW
                   else
                     if P -> . FREELOW = NIL then
                       P_VOR -> . LEN_FREELOW := 0
                     else
                       P_VOR -> . LEN_FREELOW := 8
                 end (* then *)
               else
                 begin
                   P -> . FREEEQ -> . FREELOW := P -> . FREELOW ;
                   if SIZE > 8 then
                     P -> . FREEEQ -> . LEN_FREELOW := P -> .
                                                   LEN_FREELOW ;
                   P_VOR -> . FREELOW := P -> . FREEEQ ;
                   if SIZE > 8 then
                     P_VOR -> . LEN_FREELOW := P -> . LEN_FREEEQ
                   else
                     if P -> . FREEEQ = NIL then
                       P_VOR -> . LEN_FREELOW := 0
                     else
                       P_VOR -> . LEN_FREELOW := 8
                 end (* else *) ;
             end (* tag/ca *) ;
     end (* case *) ;
   end (* REMOVE_HFRE *) ;



local procedure MODIFY_TREE ( MODUS : CHAR ; HANC_ACT : PHANC ; SIZE :
                            INTEGER ; var PLAUF : PHFRE ; PLAUFACT :
                            PACT ) ;

(****************************************************************)
(*  baum bearbeiten, zunaechst beim allokieren eines bereichs   *)
(*                                                              *)
(*  modus     = was ist zu tun ?                                *)
(*  hanc_act  = heap-Segment, in dem etwas zu tun ist           *)
(*  size      = Groesse des zu bearbeitenden Bereichs           *)
(*  plauf     = ggf. benachbarte Luecke                         *)
(*  plaufact  = ggf. freizugebender Bereich                     *)
(****************************************************************)


   var PNACHB : PHFRE ;
       PFREEN : PHFRE ;
       SIZE_ALT : INTEGER ;
       VORG_MODUS : CHAR ;
       PLAUFALT : PHFRE ;
       SIZE_FREE : INTEGER ;
       SIZE_NEU : INTEGER ;
       ADRDIFF : INTEGER ;
       PLAUF_SUCH : PHFRE ;
       VMODUS2 : CHAR ;


   procedure MODIFY_PRIOR ( PNEU : PHFRE ; VORG_MODUS : CHAR ; HANC_ACT
                          : PHANC ; PLAUFALT : PHFRE ; SIZE_FREE :
                          INTEGER ) ;

      begin (* MODIFY_PRIOR *)
        case VORG_MODUS of
          'T' : with HANC_ACT -> do
                  begin
                    if SIZE_FREE > 0 then
                      FREE1 := PNEU
                    else
                      FREE1 := NIL ;
                    LEN_FREE1 := SIZE_FREE ;
                  end (* with *) ;
          'V' : with PLAUFALT -> do
                  begin
                    if SIZE_FREE > 0 then
                      FREELOW := PNEU
                    else
                      FREELOW := NIL ;
                    LEN_FREELOW := SIZE_FREE ;
                  end (* with *) ;
        end (* case *) ;
      end (* MODIFY_PRIOR *) ;


   procedure CHAIN_PRIOR ( PNEU : PHFRE ; VORG_MODUS : CHAR ; HANC_ACT
                         : PHANC ; PLAUFALT : PHFRE ; SIZE_FREE :
                         INTEGER ) ;

      begin (* CHAIN_PRIOR *)
        case VORG_MODUS of
          'T' : begin
                  HANC_ACT -> . FREE1 := PNEU ;
                  HANC_ACT -> . LEN_FREE1 := SIZE_FREE ;
                  if PNEU = NIL then
                    HANC_ACT -> . LEN_FREE1 := 0 ;
                end (* tag/ca *) ;
          'V' : begin
                  PLAUFALT -> . FREELOW := PNEU ;
                  PLAUFALT -> . LEN_FREELOW := SIZE_FREE ;
                  if PNEU = NIL then
                    PLAUFALT -> . LEN_FREELOW := 0 ;
                end (* tag/ca *) ;
        end (* case *) ;
      end (* CHAIN_PRIOR *) ;


   procedure COPY_HFRE ( SOURCE : PHFRE ; TARGET : PHFRE ; SIZE :
                       INTEGER ) ;

      begin (* COPY_HFRE *)
        if SIZE = 0 then
          return ;
        if SIZE >= 16 then
          TARGET -> := SOURCE ->
        else
          begin
            TARGET -> . FREELOW := SOURCE -> . FREELOW ;
            TARGET -> . FREEEQ := SOURCE -> . FREEEQ ;
          end (* else *)
      end (* COPY_HFRE *) ;


   procedure INS_HFRE_EQUAL ( PFREEN : PHFRE ; SIZE_FREE : INTEGER ;
                            PFIRST : PHFRE ; PLAUFALT : PHFRE ;
                            VORG_MODUS : CHAR ) ;

   /*************************************************/
   /*   einfuegen eines neuen free-elements         */
   /*   in einer liste von gleichlangen elementen   */
   /*************************************************/


      var PLAUFF : PHFRE ;

      begin (* INS_HFRE_EQUAL *)
        if PTRDIFF ( PFREEN , PFIRST ) < 0 then
          begin

        /*******************/
        /* vorne einfuegen */
        /*******************/

            COPY_HFRE ( PFIRST , PFREEN , SIZE_FREE ) ;
            PFREEN -> . FREEEQ := PFIRST ;
            if SIZE_FREE > 8 then
              PFREEN -> . LEN_FREEEQ := SIZE_FREE ;
            PFIRST -> . FREELOW := NIL ;
            if SIZE_FREE > 8 then
              PFIRST -> . LEN_FREELOW := 0 ;
            MODIFY_PRIOR ( PFREEN , VORG_MODUS , HANC_ACT , PLAUFALT ,
                           SIZE_FREE )
          end (* then *)
        else
          begin

        /***************************/
        /* weiter hinten einfuegen */
        /***************************/

            PLAUFF := PFIRST ;
            while TRUE do
              begin
                if PLAUFF -> . FREEEQ = NIL then
                  break ;
                if PTRDIFF ( PFREEN , PLAUFF -> . FREEEQ ) < 0 then
                  break ;
                PLAUFF := PLAUFF -> . FREEEQ ;
              end (* while *) ;
            COPY_HFRE ( PLAUFF , PFREEN , SIZE_FREE ) ;
            PLAUFF -> . FREEEQ := PFREEN ;
            if SIZE_FREE > 8 then
              PLAUFF -> . LEN_FREEEQ := SIZE_FREE ;
          end (* else *) ;
      end (* INS_HFRE_EQUAL *) ;


   procedure INS_HFRE_ABOVE ( PFREEN : PHFRE ; SIZE_FREE : INTEGER ;
                            HANC_ACT : PHANC ; PLAUF : PHFRE ;
                            VORG_MODUS : CHAR ) ;

      begin (* INS_HFRE_ABOVE *)
        case VORG_MODUS of
          'N' : begin
                  PFREEN -> . FREELOW := HANC_ACT -> . FREE1 ;
                  PFREEN -> . FREEEQ := NIL ;
                  if SIZE_FREE > 8 then
                    begin
                      PFREEN -> . LEN_FREELOW := HANC_ACT -> .
                                                 LEN_FREE1 ;
                      PFREEN -> . LEN_FREEEQ := 0 ;
                    end (* then *) ;
                  HANC_ACT -> . FREE1 := PFREEN ;
                  HANC_ACT -> . LEN_FREE1 := SIZE_FREE ;
                end (* tag/ca *) ;
          'T' , 'V' :
            begin
              PFREEN -> . FREELOW := PLAUF -> . FREELOW ;
              PFREEN -> . FREEEQ := NIL ;
              if SIZE_FREE > 8 then
                begin
                  PFREEN -> . LEN_FREELOW := PLAUF -> . LEN_FREELOW ;
                  PFREEN -> . LEN_FREEEQ := 0 ;
                end (* then *) ;
              PLAUF -> . FREELOW := PFREEN ;
              PLAUF -> . LEN_FREELOW := SIZE_FREE ;
            end (* tag/ca *) ;
        end (* case *) ;
      end (* INS_HFRE_ABOVE *) ;


   procedure SUCHE_HFRE_NACH_GROESSE ;

      begin (* SUCHE_HFRE_NACH_GROE *)

        /*******************************************************/
        /* die nun folgende Schleife positioniert PLAUF auf    */
        /* das kleinste Free Element, das noch passt -         */
        /* bei ALLOC muss es eines geben aufgrund der          */
        /* Anfangsbedingung; bei FREE evtl. auch nicht         */
        /* Vorg_Modus N, d.h. nichts gefunden                  */
        /* Vorg_Modus T = top, d.h. der Vorgaenger             */
        /* der Luecke ist der HANC                             */
        /* Vorg_Modus V = vertikal, d.h. der Vorgaenger        */
        /* der Luecke ist ein anderer HFRE                     */
        /*******************************************************/

        VORG_MODUS := 'T' ;
        PLAUFALT := NIL ;
        SIZE_FREE := HANC_ACT -> . LEN_FREE1 ;
        PLAUF := HANC_ACT -> . FREE1 ;
        if SIZE_FREE < SIZE then
          begin
            VORG_MODUS := 'N' ;
            return ;
          end (* then *) ;
        while TRUE do
          with PLAUF -> do
            begin
              if FREELOW = NIL then
                break ;
              if SIZE_FREE <= 8 then
                break ;
              if LEN_FREELOW < SIZE then
                break ;
              VORG_MODUS := 'V' ;
              PLAUFALT := PLAUF ;
              SIZE_FREE := LEN_FREELOW ;
              PLAUF := FREELOW ;
            end (* with *) ;
      end (* SUCHE_HFRE_NACH_GROE *) ;


   procedure SUCHE_HFRE ;

      var PLAUFX : PHFRE ;

      begin (* SUCHE_HFRE *)
        if PLAUFALT <> NIL then
          begin
            SIZE_ALT := PLAUFALT -> . LEN_FREELOW ;
            PLAUFX := PLAUFALT -> . FREELOW ;
          end (* then *)
        else
          begin
            SIZE_ALT := HANC_ACT -> . LEN_FREE1 ;
            PLAUFALT := HANC_ACT -> . FREE1 ;
            PLAUFX := PLAUFALT ;
          end (* else *) ;
        while TRUE do
          with PLAUFX -> do
            begin

        //****************************************
        // schauen ob size von plaufx >= der      
        // geforderten groesse ist                
        // falls ja, passt diese stelle           
        //****************************************

              if SIZE_ALT <= SIZE_FREE then
                break ;

        //****************************************
        // falls nein, weiterschalten zu          
        // naechst tieferem element               
        //****************************************

              if FREELOW = NIL then
                break ;
              if SIZE_ALT > 8 then
                SIZE_ALT := LEN_FREELOW
              else
                SIZE_ALT := 0 ;
              PLAUFALT := PLAUFX ;
              PLAUFX := FREELOW ;
            end (* with *) ;
        if PLAUFALT = NIL then
          $ERROR ( 1111 ) ;
      end (* SUCHE_HFRE *) ;


   procedure SUCHE_HFRE_NACH_ADRESSE ;

      var GEFUNDEN : BOOLEAN ;

      begin (* SUCHE_HFRE_NACH_ADRE *)

        /*******************************************************/
        /* die nun folgende Schleife positioniert PLAUF auf    */
        /* das passende Free Element                           */
        /* muss vorhanden sein aufgrund der                    */
        /* Anfangsbedingung; SIZE und Adresse muessen passen   */
        /*******************************************************/

        GEFUNDEN := FALSE ;
        VORG_MODUS := 'T' ;

        /*****************************************/
        /* T = Top, d.h. HANC ist Vorgaenger     */
        /*****************************************/

        PLAUFALT := NIL ;
        SIZE_FREE := HANC_ACT -> . LEN_FREE1 ;
        SIZE_ALT := SIZE_FREE ;
        PLAUF := HANC_ACT -> . FREE1 ;
        while TRUE do
          with PLAUF -> do
            begin
              if SIZE_FREE = SIZE then
                while TRUE do
                  with PLAUF -> do
                    begin
                      if PLAUF = PLAUF_SUCH then
                        begin
                          GEFUNDEN := TRUE ;
                          break ;
                        end (* then *) ;
                      if FREEEQ = NIL then
                        break ;

        /*****************************************/
        /* H = horizontaler Vorgaenger           */
        /*****************************************/

                      VORG_MODUS := 'H' ;
                      PLAUFALT := PLAUF ;
                      PLAUF := FREEEQ ;
                    end (* with *) ;
              if GEFUNDEN then
                break ;
              if FREELOW = NIL then
                break ;
              if SIZE_FREE <= 8 then
                break ;
              if LEN_FREELOW < SIZE then
                break ;

        /*****************************************/
        /* V = vertikaler Vorgaenger             */
        /*****************************************/

              VORG_MODUS := 'V' ;
              PLAUFALT := PLAUF ;
              SIZE_ALT := SIZE_FREE ;
              SIZE_FREE := LEN_FREELOW ;
              PLAUF := FREELOW ;
            end (* with *)
      end (* SUCHE_HFRE_NACH_ADRE *) ;


   procedure INSERT_HFRE ;

   /*************************************************/
   /*   bereich pfreen und size in baum einfuegen   */
   /*************************************************/


      begin (* INSERT_HFRE *)
        SUCHE_HFRE_NACH_GROESSE ;

        /****************************************/
        /* bei gleicher laenge:                 */
        /* einsortieren nach adressen           */
        /****************************************/

        if SIZE = SIZE_FREE then
          begin
            INS_HFRE_EQUAL ( PFREEN , SIZE_FREE , PLAUF , PLAUFALT ,
                             VORG_MODUS ) ;
            return ;
          end (* then *) ;

        /****************************************/
        /* andernfalls: der neue bereich        */
        /* muss ueber dem gefundenen            */
        /* einsortiert werden (neue laenge)     */
        /****************************************/

        INS_HFRE_ABOVE ( PFREEN , SIZE , HANC_ACT , PLAUF , VORG_MODUS
                         ) ;
      end (* INSERT_HFRE *) ;


   begin (* MODIFY_TREE *)
     if TRLEVEL >= 3 then
       begin
         WRITELN ( 'modify_tree: modus = ' , MODUS ) ;
         WRITELN ( 'modify_tree: plaufact = ' , PLAUFACT ) ;
         WRITELN ( 'modify_tree: hanc_act = ' , HANC_ACT ) ;
         WRITELN ( 'modify_tree: size = ' , SIZE ) ;
         WRITELN ( 'modify_tree: plauf = ' , PLAUF ) ;
       end (* then *) ;

     /*******************************************************/
     /* Fallunterscheidung je nach Verarbeitungsmodus       */
     /*******************************************************/

     case MODUS of

     /*******************************************************/
     /* A = Alloc, d.h. neuen Bereich einfuegen (von        */
     /* Luecke wegnehmen); es ist schon klar, dass das      */
     /* klappen muss                                        */
     /*******************************************************/

       'A' : begin
               SUCHE_HFRE_NACH_GROESSE ;

     /*****************************************/
     /* Size des gefundenen Elements reduz.   */
     /*****************************************/

               SIZE_FREE := SIZE_FREE - SIZE ;

     /*****************************************/
     /* Element passt, jetzt muss der Baum    */
     /* evtl. modifiziert werden              */
     /*****************************************/

               if TRLEVEL >= 3 then
                 begin
                   WRITELN ( 'nach SUCHE_HFRE_NACH_GROESSE' ) ;
                   WRITELN ( 'modify_tree: modus = ' , MODUS ) ;
                   WRITELN ( 'modify_tree: hanc_act = ' , HANC_ACT ) ;
                   WRITELN ( 'modify_tree: size_free = ' , SIZE_FREE )
                             ;
                   WRITELN ( 'modify_tree: size = ' , SIZE ) ;
                   WRITELN ( 'modify_tree: plauf = ' , PLAUF ) ;
                   WRITELN ( 'modify_tree: plauf/len_freelow = ' ,
                             PLAUF -> . LEN_FREELOW ) ;
                   WRITELN ( 'modify_tree: plaufalt = ' , PLAUFALT ) ;
                   WRITELN ( 'modify_tree: vorg_modus = ' , VORG_MODUS
                             ) ;
                 end (* then *) ;

     /****************************************************/
     /*  modus = T(op) oder V(ertikal)                   */
     /*  top, wenn der HANC direkt auf das benutzte      */
     /*  free element zeigt, sonst vertikal              */
     /*  HANC_ACT = zeiger auf den aktuellen HANC        */
     /*  plaufalt = vorgaenger free element oder nil     */
     /*  plauf = das aktuelle (benutzte) free element    */
     /*  size_free = neue groesse (kann auch null        */
     /*              oder 8 sein)                        */
     /****************************************************/

               if PLAUF -> . LEN_FREELOW < SIZE_FREE then
                 begin
                   if TRLEVEL >= 3 then
                     WRITELN ( 'modify_tree: fall 1' ) ;

     /******************************************/
     /* sonderfall: free element ist immer     */
     /* noch groesser als das naechst tiefere  */
     /* ist auch dann der fall, wenn es        */
     /* gar kein tieferes element mehr gibt    */
     /******************************************/

                   if PLAUF -> . FREEEQ = NIL then
                     begin
                       if TRLEVEL >= 3 then
                         WRITELN ( 'modify_tree: fall 1.1' ) ;

     /***********************************/
     /* es gibt auch keine nachbarn mit */
     /* derselben laenge, d.h. das      */
     /* element aendert seinen platz    */
     /* ueberhaupt nicht                */
     /***********************************/

                       PFREEN := PTRADD ( PLAUF , SIZE ) ;
                       COPY_HFRE ( PLAUF , PFREEN , SIZE_FREE ) ;
                       MODIFY_PRIOR ( PFREEN , VORG_MODUS , HANC_ACT ,
                                      PLAUFALT , SIZE_FREE )
                     end (* then *)
                   else
                     begin
                       if TRLEVEL >= 3 then
                         WRITELN ( 'modify_tree: fall 1.2' ) ;

     /**************************************/
     /* leider gibt es nachbarn; also      */
     /* rechten nachbarn nach vorne holen  */
     /* und das geaenderte element eine    */
     /* ebene tiefer einsortieren          */
     /**************************************/

                       PNACHB := PLAUF -> . FREEEQ ;
                       CHAIN_PRIOR ( PNACHB , VORG_MODUS , HANC_ACT ,
                                     PLAUFALT , PLAUF -> . LEN_FREEEQ )
                                     ;
                       if SIZE_FREE > 0 then
                         begin
                           PFREEN := PTRADD ( PLAUF , SIZE ) ;
                           COPY_HFRE ( PLAUF , PFREEN , SIZE_FREE ) ;
                           PFREEN -> . FREEEQ := NIL ;
                           if SIZE_FREE >= 16 then
                             PFREEN -> . LEN_FREEEQ := 0 ;
                           PNACHB -> . FREELOW := PFREEN ;
                           PNACHB -> . LEN_FREELOW := SIZE_FREE ;
                         end (* then *)
                       else
                         begin
                           PNACHB -> . FREELOW := NIL ;
                           PNACHB -> . LEN_FREELOW := 0 ;
                         end (* else *)
                     end (* else *) ;
                   return
                 end (* then *) ;

     /****************************************/
     /* die neue laenge des elements ist     */
     /* kleiner oder gleich der laenge des   */
     /* elements eine ebene tiefer           */
     /****************************************/
     /* zuerst wird das element am aktuellen */
     /* platz entfernt                       */
     /****************************************/

               if PLAUF -> . FREEEQ <> NIL then
                 begin
                   if TRLEVEL >= 3 then
                     WRITELN ( 'modify_tree: fall 2.1' ) ;
                   PNACHB := PLAUF -> . FREEEQ ;
                   CHAIN_PRIOR ( PNACHB , VORG_MODUS , HANC_ACT ,
                                 PLAUFALT , PLAUF -> . LEN_FREEEQ ) ;
                   PNACHB -> . FREELOW := PLAUF -> . FREELOW ;
                   PNACHB -> . LEN_FREELOW := PLAUF -> . LEN_FREELOW ;
                 end (* then *)
               else
                 begin
                   if TRLEVEL >= 3 then
                     WRITELN ( 'modify_tree: fall 2.2' ) ;
                   PNACHB := PLAUF -> . FREELOW ;
                   CHAIN_PRIOR ( PNACHB , VORG_MODUS , HANC_ACT ,
                                 PLAUFALT , PLAUF -> . LEN_FREELOW ) ;
                 end (* else *) ;

     /****************************************/
     /* wenn die neue groesse null ist,      */
     /* sind wir schon fertig                */
     /****************************************/

               if SIZE_FREE = 0 then
                 return ;

     /****************************************/
     /* andernfalls muss das neue element    */
     /* an der passenden stelle wieder       */
     /* eingefuegt werden                    */
     /****************************************/
     /* plaufalt ist entweder nil, dann      */
     /* stehen wir noch ganz oben, oder      */
     /* plaufalt ist der vorgaenger          */
     /* von pfreen ...                       */
     /****************************************/

               if TRLEVEL >= 3 then
                 begin
                   WRITELN ( 'vor SUCHE_HFRE' ) ;
                   WRITELN ( 'modify_tree: modus = ' , MODUS ) ;
                   WRITELN ( 'modify_tree: hanc_act = ' , HANC_ACT ) ;
                   WRITELN ( 'modify_tree: size_free = ' , SIZE_FREE )
                             ;
                   WRITELN ( 'modify_tree: size = ' , SIZE ) ;
                   WRITELN ( 'modify_tree: plauf = ' , PLAUF ) ;
                   WRITELN ( 'modify_tree: plaufalt = ' , PLAUFALT ) ;
                   WRITELN ( 'modify_tree: vorg_modus = ' , VORG_MODUS
                             ) ;
                 end (* then *) ;
               SUCHE_HFRE ;
               if PLAUFALT = HANC_ACT -> . FREE1 then
                 VMODUS2 := 'T'
               else
                 VMODUS2 := 'V' ;
               if TRLEVEL >= 3 then
                 begin
                   WRITELN ( 'nach SUCHE_HFRE' ) ;
                   WRITELN ( 'modify_tree: modus = ' , MODUS ) ;
                   WRITELN ( 'modify_tree: hanc_act = ' , HANC_ACT ) ;
                   WRITELN ( 'modify_tree: size_free = ' , SIZE_FREE )
                             ;
                   WRITELN ( 'modify_tree: size = ' , SIZE ) ;
                   WRITELN ( 'modify_tree: plauf = ' , PLAUF ) ;
                   WRITELN ( 'modify_tree: plaufalt = ' , PLAUFALT ) ;
                   WRITELN ( 'modify_tree: vorg_modus = ' , VORG_MODUS
                             ) ;
                   WRITELN ( 'modify_tree: vmodus2 = ' , VMODUS2 ) ;
                 end (* then *) ;

     /****************************************/
     /* bei allen elementen vorher war       */
     /* LEN_FREELOW groesser; jetzt ist      */
     /* erstmals LEN_FREELOW kleiner gleich  */
     /* oder FREELOW nil                     */
     /****************************************/
     /* bei gleicher laenge:                 */
     /* einsortieren nach adressen           */
     /* in genau dieser liste                */
     /****************************************/
     /* bei kleinerer laenge:                */
     /* neue liste aufbauen vor der          */
     /* gefundenen liste                     */
     /****************************************/
     /* in beiden faellen ist vmodus2        */
     /* zu beachten (d.h. das gefundene      */
     /* plaufalt koennte direkt am           */
     /* hanc dranhaengen)                    */
     /****************************************/
     /* das ist m.E. die einzige Stelle,     */
     /* wo uebrigbleibende Luecken der       */
     /* Laenge 8 entstehen koennen; diese    */
     /* werden auch nie mehr recyclet        */
     /****************************************/

               if SIZE_ALT = SIZE_FREE then
                 begin
                   PFREEN := PTRADD ( PLAUF , SIZE ) ;
                   COPY_HFRE ( PLAUF , PFREEN , SIZE_FREE ) ;
                   INS_HFRE_EQUAL ( PFREEN , SIZE_FREE , PLAUFALT ,
                                    PLAUFALT , VMODUS2 ) ;
                   return ;
                 end (* then *) ;

     /****************************************/
     /* andernfalls: der neue bereich        */
     /* muss ueber dem gefundenen            */
     /* einsortiert werden (neue laenge)     */
     /****************************************/

               PFREEN := PTRADD ( PLAUF , SIZE ) ;
               COPY_HFRE ( PLAUF , PFREEN , SIZE_FREE ) ;
               INS_HFRE_ABOVE ( PFREEN , SIZE , NIL , PLAUFALT ,
                                VMODUS2 ) ;
             end (* tag/ca *) ;

     /*******************************************************/
     /* F = Free, d.h. Bereich freigeben                    */
     /* ohne Zusammenlegen (neue Luecke)                    */
     /*******************************************************/

       'F' : begin
               PFREEN := PTRCAST ( PLAUFACT ) ;
               INSERT_HFRE ;
             end (* tag/ca *) ;

     /*******************************************************/
     /* Zusammenlegen von Bereichen;                        */
     /* Vorgabe von alter Adresse, neuer Adresse und        */
     /* neuer Laenge                                        */
     /*******************************************************/

       'Z' : begin
               SIZE_NEU := SIZE ;
               PLAUF_SUCH := PLAUF ;
               ADRDIFF := PTRDIFF ( PLAUFACT , PLAUF ) ;
               if ADRDIFF > 0 then
                 begin
                   PFREEN := PLAUF ;
                   SIZE := ADRDIFF ;
                 end (* then *)
               else
                 begin
                   PFREEN := PTRCAST ( PLAUFACT ) ;
                   SIZE_FREE := - ADRDIFF ;
                   SIZE := SIZE_NEU - SIZE_FREE ;
                 end (* else *) ;
               SUCHE_HFRE_NACH_ADRESSE ;
               if PLAUF <> PLAUF_SUCH then
                 $ERROR ( 1100 ) ;

     /*******************************************************/
     /* der neu freizugebende Bereich liegt links           */
     /* am bestehenden Bereich; plauf suchen und            */
     /* bereich entsprechend verlaengern. adrdiff ist       */
     /* die bestehende groesse des bereichs - aber negativ  */
     /*******************************************************/

               if ( ( VORG_MODUS = 'T' ) and ( PLAUF -> . FREEEQ = NIL
               ) ) or ( ( VORG_MODUS = 'V' ) and ( PLAUF -> . FREEEQ =
               NIL ) and ( SIZE_NEU < SIZE_ALT ) ) then
                 begin

     /*******************************************************/
     /* Sonderfall: Bereich kann an dieser Stelle           */
     /* bleiben                                             */
     /*******************************************************/

                   if TRLEVEL >= 3 then
                     begin
                       WRITELN ( 'Zusammenlegung am Platz' ) ;
                     end (* then *) ;
                   if ADRDIFF < 0 then
                     COPY_HFRE ( PLAUF , PFREEN , SIZE_FREE ) ;
                   MODIFY_PRIOR ( PFREEN , VORG_MODUS , HANC_ACT ,
                                  PLAUFALT , SIZE_NEU ) ;
                 end (* then *)
               else
                 begin

     /*****************/
     /* raus und rein */
     /*****************/

                   if TRLEVEL >= 3 then
                     begin
                       WRITELN ( 'komplizierte Zusammenlegung' ) ;
                       WRITELN ( 'vorg_modus = ' , VORG_MODUS ) ;
                       WRITELN ( 'plauf = ' , PLAUF ) ;
                       WRITELN ( 'plaufalt = ' , PLAUFALT ) ;
                       WRITELN ( 'size = ' , SIZE ) ;
                     end (* then *) ;
                   REMOVE_HFRE ( VORG_MODUS , PLAUF , PLAUFALT ,
                                 HANC_ACT , SIZE ) ;
                   if TRLEVEL >= 3 then
                     begin
                       WRITELN (
                           'Zustand zwischen Loeschung und Einfuegung'
                                 ) ;
                       LISTE_DER_LUECKEN ( HANC_ACT ) ;
                     end (* then *) ;
                   SIZE := SIZE_NEU ;
                   INSERT_HFRE ;
                 end (* else *)
             end (* tag/ca *) ;

     /*******************************************************/
     /* andere Faelle gibt's nicht                          */
     /*******************************************************/

       otherwise
         begin
           
         end (* otherw *) ;
     end (* case *) ;
   end (* MODIFY_TREE *) ;



local function ALLOC_AREA ( SIZE : INTEGER ) : VOIDPTR ;

(****************************************************************)
(*  works much the same as C malloc                             *)
(****************************************************************)


   var SIZEX : INTEGER ;
       XP : PACT ;
       HANC_ACT : PHANC ;
       PLAUF : PHFRE ;
       HANC_NEW : PHANC ;
       HANC_CHK : PHANC ;
       HANC_CHK2 : PHANC ;
       HANC_LAST : PHANC ;
       HANC_FWD : PHANC ;
       HANC_BWD : PHANC ;
       PHEAPC : PHPCB ;

   begin (* ALLOC_AREA *)

     (****************************************************)
     (*  Size aufrunden auf durch 8 teilbar              *)
     (****************************************************)

     SIZE := ( ( SIZE + 7 ) DIV 8 ) * 8 + 8 ;

     (****************************************************)
     (*  Pointer auf Heap Control Block besorgen         *)
     (****************************************************)

     if PHEAP = NIL then
       begin
         PHEAP := ADDR ( HEAPCB ) ;
         PHEAP -> . EYECATCH := 'HPCB' ;
         PHEAP -> . FIRST := NIL ;
         PHEAP -> . LAST := NIL ;
         PHEAP -> . ACTIVE := 1 ;
       end (* then *) ;
     PHEAPC := PHEAP ;

     /********************************************/
     /* traceausgaben und ggf.                   */
     /* existierende hancs ueberpruefen          */
     /********************************************/

     if TRLEVEL >= 1 then
       begin
         WRITELN ( '--------------------------' ,
                   '-----------------------' ) ;
         WRITELN ( 'alloc_area: size = ' , SIZE ) ;
         if TRLEVEL >= 3 then
           LISTE_ALLER_HANCS ( PHEAPC , FALSE ) ;
       end (* then *) ;

     (****************************************************)
     (*  Suchen in der Liste der vorhandenen HANCs,      *)
     (*  ob es einen mit einer passenden Luecke gibt;    *)
     (*  Start mit PHEAPCC ->. LAST                      *)
     (*                                                  *)
     (****************************************************)
     (*  wenn passender HANC gefunden, wird dieser       *)
     (*  an das Ende der Liste geholt (damit voll        *)
     (*  belegte HANCs diesen Platz freimachen)          *)
     (****************************************************)

     HANC_ACT := NIL ;
     HANC_CHK := PHEAPC -> . LAST ;
     if HANC_CHK <> NIL then
       if HANC_CHK -> . LEN_FREE1 >= SIZE then
         begin
           HANC_ACT := HANC_CHK ;
         end (* then *)
       else
         begin
           HANC_CHK2 := PHEAPC -> . FIRST ;
           while HANC_CHK2 <> HANC_CHK do
             begin
               if HANC_CHK2 -> . LEN_FREE1 >= SIZE then
                 begin
                   HANC_LAST := PHEAPC -> . LAST ;
                   HANC_FWD := HANC_CHK2 -> . FWD ;
                   if PHEAPC -> . FIRST = HANC_CHK2 then
                     begin
                       HANC_FWD -> . BWD := PTRCAST ( PHEAPC ) ;
                       PHEAPC -> . FIRST := HANC_FWD ;
                     end (* then *)
                   else
                     begin
                       HANC_BWD := HANC_CHK2 -> . BWD ;
                       HANC_FWD -> . BWD := HANC_BWD ;
                       HANC_BWD -> . FWD := HANC_FWD ;
                     end (* else *) ;
                   HANC_CHK2 -> . FWD := PTRCAST ( PHEAPC ) ;
                   HANC_CHK2 -> . BWD := HANC_LAST ;
                   HANC_LAST -> . FWD := HANC_CHK2 ;
                   PHEAPC -> . LAST := HANC_CHK2 ;
                   HANC_ACT := HANC_CHK2 ;
                   if TRLEVEL >= 2 then
                     WRITELN ( 'alloc_area: hanc = ' , HANC_ACT ,
                               ' size_free1 = ' , HANC_ACT -> .
                               LEN_FREE1 ) ;
                   break
                 end (* then *) ;
               HANC_CHK2 := HANC_CHK2 -> . FWD
             end (* while *)
         end (* else *) ;

     (****************************************************)
     (*  wenn HANC_ACT hier ungleich NIL ist,            *)
     (*  passt er auch (d.h. Luecke gross genug)         *)
     (****************************************************)

     if HANC_ACT <> NIL then
       begin
         with HANC_ACT -> do
           begin

     /*************************************************/
     /* XP = PLAUF = Anfangsadresse des neuen         */
     /* Bereichs; der Rest wird als neuer freier      */
     /* Bereich in den Baum einsortiert               */
     /*************************************************/

             if TRLEVEL >= 3 then
               begin
                 WRITELN ( 'alloc_area: vor modify_tree' ) ;
                 WRITELN ( 'alloc_area: hanc.free1 = ' , FREE1 ) ;
                 WRITELN ( 'alloc_area: hanc.len_free1 = ' , LEN_FREE1
                           ) ;
               end (* then *) ;
             PLAUF := NIL ;
             MODIFY_TREE ( 'A' , HANC_ACT , SIZE , PLAUF , NIL ) ;
             XP := PTRCAST ( PLAUF ) ;
             XP -> . XHANC := HANC_ACT ;
             XP -> . XSIZE := SIZE ;
             if TRLEVEL >= 1 then
               begin
                 if TRLEVEL >= 2 then
                   begin
                     if TRLEVEL >= 3 then
                       begin
                         WRITELN ( 'alloc_area: nach modify_tree' ) ;
                         WRITELN ( 'alloc_area: hanc.free1 = ' , FREE1
                                   ) ;
                         WRITELN ( 'alloc_area: hanc.len_free1 = ' ,
                                   LEN_FREE1 ) ;
                       end (* then *) ;
                     LISTE_DER_LUECKEN ( HANC_ACT ) ;
                     if TRLEVEL >= 3 then
                       begin
                         LISTE_ALLER_HANCS ( PHEAPC , FALSE ) ;
                         DUMP ( HANC_ACT , PTRADD ( HANC_ACT , 256 ) )
                       end (* then *)
                   end (* then *) ;
                 WRITELN ( 'alloc_area: Rueckgabe = ' , PTRADD ( XP , 8
                           ) ) ;
                 WRITELN ( '--------------------------' ,
                           '-----------------------' ) ;
               end (* then *) ;
             ALLOC_AREA := PTRADD ( XP , 8 ) ;
             return
           end (* with *) ;
       end (* then *) ;

     (****************************************************)
     (*  wenn wir hier ankommen, brauchen wir auf        *)
     (*  jeden Fall einen neuen HANC                     *)
     (****************************************************)

     SIZEX := HANCSIZE ;
     if SIZEX < SIZE + 32 then
       SIZEX := SIZE + 32 ;
     HANC_NEW := PTRCAST ( ALLOC_AREAX ( SIZEX ) ) ;
     if HANC_NEW = NIL then
       $ERROR ( 1104 ) ;

     (**********************************)
     (*  hanc mit werten belegen       *)
     (**********************************)

     with HANC_NEW -> do
       begin
         EYECATCH := 'HANC' ;
         FWD := PTRCAST ( PHEAPC ) ;
         BWD := PTRCAST ( PHEAPC ) ;
         DUMMY := 0 ;
         AREA1 := PTRADD ( HANC_NEW , 32 ) ;
         LEN_FREE1 := SIZEX - 32 - SIZE ;
         if LEN_FREE1 > 0 then
           begin
             FREE1 := PTRADD ( AREA1 , SIZE ) ;

     (***************************************)
     (*  hfre element entsprechend anlegen  *)
     (***************************************)

             PLAUF := FREE1 ;
             with PLAUF -> do
               begin
                 FREELOW := NIL ;
                 FREEEQ := NIL ;
                 if LEN_FREE1 >= 16 then
                   begin
                     LEN_FREELOW := 0 ;
                     LEN_FREEEQ := 0 ;
                   end (* then *)
               end (* with *)
           end (* then *)
         else
           FREE1 := NIL ;
         LEN_AREA1 := SIZEX - 32 ;
       end (* with *) ;

     (**********************************)
     (*  hanc verpointern              *)
     (**********************************)

     if PHEAPC -> . LAST = NIL then
       begin
         PHEAPC -> . FIRST := HANC_NEW ;
         PHEAPC -> . LAST := HANC_NEW ;
         HANC_NEW -> . BWD := PTRCAST ( PHEAPC ) ;
         HANC_NEW -> . FWD := PTRCAST ( PHEAPC ) ;
       end (* then *)
     else
       begin
         PHEAPC -> . LAST -> . FWD := HANC_NEW ;
         HANC_NEW -> . BWD := PHEAPC -> . LAST ;
         HANC_NEW -> . FWD := PTRCAST ( PHEAPC ) ;
         PHEAPC -> . LAST := HANC_NEW ;
       end (* else *) ;

     (**********************************)
     (*  rueckgabe der adressen        *)
     (**********************************)

     if TRLEVEL >= 2 then
       begin
         WRITELN ( 'alloc_area: neuer hanc_act !!!' ) ;
         WRITELN ( 'alloc_area: heapc.eyecatch = ' , PHEAPC -> .
                   EYECATCH ) ;
         WRITELN ( 'alloc_area: heapc.last = ' , PHEAPC -> . LAST ) ;
       end (* then *) ;
     HANC_ACT := HANC_NEW ;
     XP := PTRCAST ( HANC_ACT -> . AREA1 ) ;
     XP -> . XHANC := HANC_ACT ;
     XP -> . XSIZE := SIZE ;
     if TRLEVEL >= 1 then
       begin
         if TRLEVEL >= 2 then
           begin
             LISTE_DER_LUECKEN ( HANC_ACT ) ;
             if TRLEVEL >= 3 then
               begin
                 LISTE_ALLER_HANCS ( PHEAPC , FALSE ) ;
                 DUMP ( HANC_ACT , PTRADD ( HANC_ACT , 256 ) )
               end (* then *)
           end (* then *) ;
         WRITELN ( 'alloc_area: Rueckgabe = ' , PTRADD ( XP , 8 ) ) ;
         WRITELN ( '--------------------------' ,
                   '-----------------------' ) ;
       end (* then *) ;
     ALLOC_AREA := PTRADD ( XP , 8 ) ;
   end (* ALLOC_AREA *) ;



local procedure FREE_AREAX ( PTR : VOIDPTR ) ;

(****************************************************************)
(*  works much the same as C free                               *)
(*  calls the OPSYS service (aka FREEMAIN) directly             *)
(****************************************************************)


   var X : VOIDPTR ;

   begin (* FREE_AREAX *)
     X := $PASSYS ( 2 , PTR ) ;
   end (* FREE_AREAX *) ;



local function CHECK_AREA_ALLOC ( PTR : VOIDPTR ) : VOIDPTR ;

(****************************************************************)
(*  checks, if ptr points to a alloc area                       *)
(****************************************************************)


   var SIZEX : INTEGER ;
       XP : PACT ;
       HANC_ACT : PHANC ;
       PTR_ENDE : VOIDPTR ;
       PHEAPC : PHPCB ;
       ERG : VOIDPTR ;
       HANC_CHK : PHANC ;
       GEFUNDEN : BOOLEAN ;

   begin (* CHECK_AREA_ALLOC *)
     ERG := NIL ;

     (****************************************************)
     (*  Pointer auf Heap Control Block besorgen         *)
     (****************************************************)

     if PHEAP = NIL then
       begin
         PHEAP := ADDR ( HEAPCB ) ;
         PHEAP -> . EYECATCH := 'HPCB' ;
         PHEAP -> . FIRST := NIL ;
         PHEAP -> . LAST := NIL ;
         PHEAP -> . ACTIVE := 1 ;
       end (* then *) ;
     PHEAPC := PHEAP ;

     /********************************************/
     /* traceausgaben und ggf.                   */
     /* existierende hancs ueberpruefen          */
     /********************************************/

     if TRLEVEL >= 1 then
       begin
         WRITELN ( '--------------------------' ,
                   '-----------------------' ) ;
         WRITELN ( 'check_area_alloc: ptr = ' , PTR ) ;
         if TRLEVEL >= 3 then
           begin
             LISTE_ALLER_HANCS ( PHEAPC , FALSE ) ;
           end (* then *)
       end (* then *) ;
     repeat

     /********************************************/
     /* checken, ob die adresse in einem         */
     /* der hancs liegt                          */
     /********************************************/

       HANC_CHK := PHEAPC -> . FIRST ;
       if HANC_CHK = NIL then
         break ;
       GEFUNDEN := FALSE ;
       while HANC_CHK <> PTRCAST ( PHEAPC ) do
         begin
           if ( PTRDIFF ( PTR , HANC_CHK -> . AREA1 ) >= 0 ) and (
           PTRDIFF ( PTR , PTRADD ( HANC_CHK -> . AREA1 , HANC_CHK -> .
           LEN_AREA1 ) ) <= 0 ) then
             begin
               GEFUNDEN := TRUE ;
               break
             end (* then *) ;
           HANC_CHK := HANC_CHK -> . FWD
         end (* while *) ;
       if not GEFUNDEN then
         break ;

     /********************************************/
     /* pruefen, ob adresse anfang eines         */
     /* alloc-bereiches                          */
     /********************************************/

       XP := PTRADD ( PTR , - 8 ) ;
       HANC_ACT := XP -> . XHANC ;
       SIZEX := XP -> . XSIZE ;
       if HANC_ACT = NIL then
         break ;
       if HANC_ACT <> HANC_CHK then
         begin
           ERG := PTRCAST ( HANC_CHK ) ;
           break ;
         end (* then *) ;
       with HANC_ACT -> do
         begin
           PTR_ENDE := PTRADD ( XP , SIZEX ) ;
           if TRLEVEL >= 3 then
             begin
               WRITELN ( 'check_area_alloc: xp = ' , XP ) ;
               WRITELN ( 'check_area_alloc: sizex = ' , SIZEX ) ;
               WRITELN ( 'check_area_alloc: hanc_act = ' , HANC_ACT ) ;
               WRITELN ( 'check_area_alloc: ptr_ende = ' , PTR_ENDE ) ;
               WRITELN ( 'check_area_alloc: ende_area1 = ' , PTRADD (
                         AREA1 , LEN_AREA1 ) ) ;
             end (* then *) ;

     /********************************************/
     /* nur, wenn wir einen hanc finden und      */
     /* der freizugebende bereich innerhalb      */
     /* der hanc grenzen ist                     */
     /********************************************/

           if EYECATCH <> 'HANC' then
             break ;
           if PTRDIFF ( PTRADD ( AREA1 , LEN_AREA1 ) , PTR_ENDE ) < 0
           then
             break ;
           if PTRDIFF ( AREA1 , XP ) > 0 then
             break ;
         end (* with *) ;
       ERG := PTRCAST ( PTR ) ;
     until TRUE ;
     CHECK_AREA_ALLOC := ERG
   end (* CHECK_AREA_ALLOC *) ;



local procedure FREE_AREA ( PTR : VOIDPTR ) ;

(****************************************************************)
(*  works much the same as C free                               *)
(****************************************************************)


   var SIZEX : INTEGER ;
       XP : PACT ;
       HANC_ACT : PHANC ;
       PTR_ENDE : VOIDPTR ;
       PLINKS : PHFRE ;
       PRECHTS : PHFRE ;
       PLINKS_VOR : PHFRE ;
       PRECHTS_VOR : PHFRE ;
       VRECHTS : CHAR ;
       VLINKS : CHAR ;
       SIZE_LINKS : INTEGER ;
       SIZE_RECHTS : INTEGER ;
       PFREE : PHFRE ;
       SIZE_FREE : INTEGER ;
       PFREE2 : PHFRE ;
       PFREE2_ENDE : VOIDPTR ;
       PDUMMY : PHFRE ;
       PHEAPC : PHPCB ;

   begin (* FREE_AREA *)
     PDUMMY := NIL ;

     (****************************************************)
     (*  Pointer auf Heap Control Block besorgen         *)
     (****************************************************)

     if PHEAP = NIL then
       begin
         PHEAP := ADDR ( HEAPCB ) ;
         PHEAP -> . EYECATCH := 'HPCB' ;
         PHEAP -> . FIRST := NIL ;
         PHEAP -> . LAST := NIL ;
         PHEAP -> . ACTIVE := 1 ;
       end (* then *) ;
     PHEAPC := PHEAP ;

     /********************************************/
     /* traceausgaben und ggf.                   */
     /* existierende hancs ueberpruefen          */
     /********************************************/

     if ( TRLEVEL >= 1 ) or FALSE then
       begin
         WRITELN ( '--------------------------' ,
                   '-----------------------' ) ;
         WRITELN ( 'free_area: ptr = ' , PTR ) ;
         if TRLEVEL >= 3 then
           begin
             LISTE_ALLER_HANCS ( PHEAPC , FALSE ) ;
           end (* then *)
       end (* then *) ;

     /********************************************/
     /* wir checken erst mal, ob eine freigabe   */
     /* ueberhaupt funktionieren kann            */
     /********************************************/

     XP := PTRADD ( PTR , - 8 ) ;
     HANC_ACT := XP -> . XHANC ;
     SIZEX := XP -> . XSIZE ;
     if HANC_ACT = NIL then
       $ERROR ( 1106 ) ;
     with HANC_ACT -> do
       begin
         PTR_ENDE := PTRADD ( XP , SIZEX ) ;
         if ( TRLEVEL >= 3 ) or FALSE then
           begin
             WRITELN ( 'free_area: xp = ' , XP ) ;
             WRITELN ( 'free_area: sizex = ' , SIZEX ) ;
             WRITELN ( 'free_area: ptr_ende = ' , PTR_ENDE ) ;
             WRITELN ( 'free_area: ende_area1 = ' , PTRADD ( AREA1 ,
                       LEN_AREA1 ) ) ;
           end (* then *) ;

     /********************************************/
     /* nur, wenn wir einen hanc finden und      */
     /* der freizugebende bereich innerhalb      */
     /* der hanc grenzen ist                     */
     /********************************************/

         if EYECATCH <> 'HANC' then
           $ERROR ( 1107 ) ;
         if PTRDIFF ( PTRADD ( AREA1 , LEN_AREA1 ) , PTR_ENDE ) < 0
         then
           $ERROR ( 1108 ) ;
         if PTRDIFF ( AREA1 , XP ) > 0 then
           $ERROR ( 1109 ) ;

     /********************************************/
     /* wir schauen jetzt alle freien bereiche   */
     /* an. wenn der freizugebende bereich       */
     /* die bestehenden luecken ueberlappt,      */
     /* haben wir ein problem                    */
     /********************************************/

         PLINKS_VOR := NIL ;
         PRECHTS_VOR := NIL ;
         PLINKS := NIL ;
         PRECHTS := NIL ;
         VRECHTS := 'T' ;
         VLINKS := 'T' ;
         SIZE_LINKS := 0 ;
         SIZE_RECHTS := 0 ;
         PFREE := FREE1 ;
         SIZE_FREE := LEN_FREE1 ;
         while PFREE <> NIL do
           with PFREE -> do
             begin
               PFREE2 := PFREE ;
               while PFREE2 <> NIL do
                 with PFREE2 -> do
                   begin
                     PFREE2_ENDE := PTRADD ( PFREE2 , SIZE_FREE ) ;

     /********************************************/
     /* pruefen, ob luecke ueberlappt oder       */
     /* an bestehende luecke vorne oder          */
     /* hinten angrenzt                          */
     /********************************************/

                     if ( PTRDIFF ( PFREE2 , PTR_ENDE ) < 0 ) and (
                     PTRDIFF ( XP , PFREE2_ENDE ) < 0 ) then
                       $ERROR ( 1110 ) ;

     /********************************************/
     /* wenn pfree2 > ptr_ende, kann man raus    */
     /* weil die ptr in der queue aufsteigend    */
     /* sortiert sind                            */
     /********************************************/

                     if PTRDIFF ( PFREE2 , PTR_ENDE ) > 0 then
                       break ;

     /********************************************/
     /* pruefen, ob luecke ueberlappt oder       */
     /* an bestehende luecke vorne oder          */
     /* hinten angrenzt                          */
     /********************************************/

                     if ( PTRCAST ( PFREE2 ) = PTR_ENDE ) then
                       begin
                         PRECHTS := PFREE2 ;
                         SIZE_RECHTS := SIZE_FREE ;
                         break ;
                       end (* then *) ;
                     if ( PTRCAST ( PFREE2_ENDE ) = XP ) then
                       begin
                         PLINKS := PFREE2 ;
                         SIZE_LINKS := SIZE_FREE
                       end (* then *) ;

     /********************************************/
     /* weiter zur naechsten luecke horizontal   */
     /********************************************/

                     if PRECHTS = NIL then
                       begin
                         PRECHTS_VOR := PFREE2 ;
                         VRECHTS := 'H'
                       end (* then *) ;
                     if PLINKS = NIL then
                       begin
                         PLINKS_VOR := PFREE2 ;
                         VLINKS := 'H'
                       end (* then *) ;
                     PFREE2 := FREEEQ ;
                   end (* with *) ;

     /********************************************/
     /* weiter zur naechsten luecke vertikal     */
     /********************************************/

               if PRECHTS = NIL then
                 begin
                   PRECHTS_VOR := PFREE ;
                   VRECHTS := 'V'
                 end (* then *) ;
               if PLINKS = NIL then
                 begin
                   PLINKS_VOR := PFREE ;
                   VLINKS := 'V'
                 end (* then *) ;
               if SIZE_FREE > 8 then
                 SIZE_FREE := LEN_FREELOW
               else
                 if FREELOW <> NIL then
                   $ERROR ( 1112 ) ;
               PFREE := FREELOW ;
             end (* with *) ;

     /********************************************/
     /* jetzt kann man die neue luecke           */
     /* einfuegen, ggf. zusammengelegt mit       */
     /* Plinks und Prechts                       */
     /********************************************/

         if TRLEVEL >= 3 then
           begin
             WRITELN ( 'free_area: Plinks = ' , PLINKS ) ;
             WRITELN ( 'free_area: size_links = ' , SIZE_LINKS ) ;
             WRITELN ( 'free_area: Prechts = ' , PRECHTS ) ;
             WRITELN ( 'free_area: size_rechts = ' , SIZE_RECHTS )
           end (* then *) ;

     /********************************************/
     /* Fallunterscheidung wg. Zusammenlegung    */
     /********************************************/

         if PLINKS = NIL then
           if PRECHTS = NIL then
             begin

     /*************************************************/
     /* nur neuen Bereich einfuegen, keine Zus.legung */
     /*************************************************/

               MODIFY_TREE ( 'F' , HANC_ACT , SIZEX , PDUMMY , XP ) ;
             end (* then *)
           else
             begin

     /*****************************************************/
     /* neuer Bereich mit daranhaengendem rechten Bereich */
     /* d.h. Bereich PRECHTS suchen und nach links        */
     /* verlaengern                                       */
     /*****************************************************/

               MODIFY_TREE ( 'Z' , HANC_ACT , SIZEX + SIZE_RECHTS ,
                             PRECHTS , XP ) ;
             end (* else *)
         else
           if PRECHTS = NIL then
             begin

     /******************************************************/
     /* neuer Bereich haengt an bereits vorhandener Luecke */
     /* diese vergroessert sich                            */
     /* d.h. Bereich PLINKS suchen und nach rechts         */
     /* verlaengern                                        */
     /******************************************************/

               MODIFY_TREE ( 'Z' , HANC_ACT , SIZEX + SIZE_LINKS ,
                             PLINKS , XP ) ;
             end (* then *)
           else
             begin

     /******************************************************/
     /* neuer Bereich liegt zwischen zwei Luecken          */
     /* alle drei zusammenfuegen                           */
     /******************************************************/
     /* zunaechst luecke rechts aus baum entfernen         */
     /* dann Bereich PLINKS suchen und nach rechts         */
     /* verlaengern                                        */
     /******************************************************/

               REMOVE_HFRE ( VRECHTS , PRECHTS , PRECHTS_VOR , HANC_ACT
                             , SIZE_RECHTS ) ;
               MODIFY_TREE ( 'Z' , HANC_ACT , SIZEX + SIZE_LINKS +
                             SIZE_RECHTS , PLINKS , XP ) ;
             end (* else *)
       end (* with *) ;
     if TRLEVEL >= 1 then
       begin
         if TRLEVEL >= 2 then
           begin
             LISTE_DER_LUECKEN ( HANC_ACT ) ;
             if TRLEVEL >= 3 then
               begin
                 LISTE_ALLER_HANCS ( PHEAPC , FALSE ) ;
                 DUMP ( HANC_ACT , PTRADD ( HANC_ACT , 256 ) )
               end (* then *)
           end (* then *) ;
         WRITELN ( '--------------------------' ,
                   '-----------------------' ) ;
       end (* then *) ;
   end (* FREE_AREA *) ;



function $PASMEM ( FUNCCODE : INTEGER ; X : VOIDPTR ) : VOIDPTR ;

(**************************************)
(*  Verteiler fuer Memory-Funktionen  *)
(**************************************)


   begin (* $PASMEM *)
     case FUNCCODE of

     /****************************/
     /* alloc lt. le mechanismus */
     /****************************/

       1 : begin
             ANZ_ALLOCS := ANZ_ALLOCS + 1 ;
             $PASMEM := ALLOC_AREA ( PTR2INT ( X ) ) ;
           end (* tag/ca *) ;

     /**********************************/
     /* allocx, d.h. alloc via getmain */
     /**********************************/

       2 : begin
             $PASMEM := ALLOC_AREAX ( PTR2INT ( X ) ) ;
           end (* tag/ca *) ;

     /***************************/
     /* free lt. le mechanismus */
     /***************************/

       3 : begin
             ANZ_FREES := ANZ_FREES + 1 ;
             FREE_AREA ( X ) ;
             $PASMEM := NIL
           end (* tag/ca *) ;

     /************************/
     /* freex, d.h. freemain */
     /************************/

       4 : begin
             FREE_AREAX ( X ) ;
             $PASMEM := NIL
           end (* tag/ca *) ;

     /*********************************/
     /* liste und checken aller hancs */
     /*********************************/

       5 : begin
             WRITELN ( '*** Start CHKHEAP ***' ) ;
             WRITELN ( 'Anzahl ALLOCs ...: ' , ANZ_ALLOCS ) ;
             WRITELN ( 'Anzahl FREEs ....: ' , ANZ_FREES ) ;
             WRITELN ( 'PHEAP ...........: ' , PHEAP ) ;
             LISTE_ALLER_HANCS ( PHEAP , TRUE ) ;
             WRITELN ( '*** Ende CHKHEAP ****' ) ;
             $PASMEM := NIL
           end (* tag/ca *) ;

     /*******************************************/
     /* checkalloc = pruefen, ob Adresse ok ist */
     /*******************************************/

       6 : begin
             $PASMEM := CHECK_AREA_ALLOC ( X ) ;
           end (* tag/ca *) ;

     /******************************************************/
     /* zu einem file den pascal-fcb inkl. dcb herausgeben */
     /******************************************************/

       7 : begin
             X := PTRCAST ( $PASSYS ( 10 , X ) ) ;
             $PASMEM := X ;
           end (* tag/ca *) ;

     /*********************************/
     /* dispose does nothing          */
     /*********************************/

       8 : begin
             WRITELN ( '*** DISPOSE has been called ***' ) ;
             WRITELN ( '*** Argument for DISPOSE = ' , X ) ;
             WRITELN ( '*** DISPOSE has no effect ***' ) ;
             $PASMEM := NIL
           end (* tag/ca *) ;
       otherwise
         $PASMEM := NIL
     end (* case *) ;
   end (* $PASMEM *) ;





(**********************************************************)
(*$D-,A+                                                  *)
(**********************************************************)
(*                                                        *)
(*  Mathematik-Funktionen fuer Pascal                     *)
(*                                                        *)
(*  02.2017 - bernd.oppolzer@yahoo.com                    *)
(*                                                        *)
(**********************************************************)
(*                                                        *)
(*  ROUNDX                                                *)
(*                                                        *)
(**********************************************************)




local function ROUNDX1 ( WERT : REAL ; BEREICH : INTEGER ) : REAL ;

(**********************************************************)
(*                                                        *)
(*   roundx.c                                             *)
(*                                                        *)
(*   Rundungsfunktion neu mit geaenderter Logik;          *)
(*   die Korrekturkonstante wird anhand der Groessen-     *)
(*   ordnung des Ausgangswertes bestimmt (Ausgangs-       *)
(*   wert durch (16 hoch 13); damit wird bei beiden       *)
(*   Plattformen mindestens eine 1 an der letzten         *)
(*   Ziffernposition dazuaddiert).                        *)
(*                                                        *)
(*   Autor: Bernd Oppolzer                                *)
(*          April 1995                                    *)
(*                                                        *)
(**********************************************************)


   var FAKTOR : REAL ;
       TEST : REAL ;
       RUNDKONST : REAL ;

   const FAKTTAB : array [ 0 .. 19 ] of REAL =
         ( 10000.0 , 1000.0 , 100.0 , 10.0 , 1.0 , 10.0 , 100.0 ,
           1000.0 , 10000.0 , 100000.0 , 1000000.0 , 10000000.0 ,
           100000000.0 , 1000000000.0 , 10000000000.0 , 100000000000.0
           , 1000000000000.0 , 10000000000000.0 , 100000000000000.0 ,
           1000000000000000.0 ) ;

   begin (* ROUNDX1 *)
     FAKTOR := FAKTTAB [ 4 - BEREICH ] ;
     if WERT < 0.0 then
       TEST := - WERT
     else
       TEST := WERT ;
     if TEST < 1.0E-55 then
       begin
         ROUNDX1 := 0.0 ;
         return
       end (* then *) ;

     /************************************************/
     /*                                              */
     /*   4 * (16 hoch 12)  =  1125899906842624.0    */
     /*   8 * (16 hoch 12)  =  2251799813685248.0    */
     /*  12 * (16 hoch 12)  =  3377699720527872.0    */
     /*        16 hoch 13   =  4503599627370496.0    */
     /*                                              */
     /************************************************/

     RUNDKONST := TEST / 1125899906842624.0 ;
     if BEREICH < 0 then
       begin
         TEST := ( TEST + RUNDKONST ) * FAKTOR + 0.5 ;
         TEST := FLOOR ( TEST ) ;
         if WERT < 0.0 then
           ROUNDX1 := - TEST / FAKTOR
         else
           ROUNDX1 := TEST / FAKTOR
       end (* then *)
     else
       if BEREICH > 0 then
         begin
           TEST := ( TEST + RUNDKONST ) / FAKTOR + 0.5 ;
           TEST := FLOOR ( TEST ) ;
           if WERT < 0.0 then
             ROUNDX1 := - TEST * FAKTOR
           else
             ROUNDX1 := TEST * FAKTOR ;
         end (* then *)
       else
         begin
           TEST := ( TEST + RUNDKONST ) + 0.5 ;
           TEST := FLOOR ( TEST ) ;
           if WERT < 0.0 then
             ROUNDX1 := - TEST
           else
             ROUNDX1 := TEST ;
         end (* else *)
   end (* ROUNDX1 *) ;



function $PASMAT ( FUNCCODE : INTEGER ; I : INTEGER ; X : REAL ) : REAL
                 ;

(*************************************)
(*  Verteiler fuer Mathe-Funktionen  *)
(*************************************)


   begin (* $PASMAT *)
     case FUNCCODE of

     /*********************************/
     /* roundx                        */
     /*********************************/

       1 : begin
             $PASMAT := ROUNDX1 ( X , I ) ;
           end (* tag/ca *) ;
       otherwise
         $ERROR ( 1120 ) ;
     end (* case *) ;
   end (* $PASMAT *) ;





(***************************************************************)
(*                                                             *)
(*  Interface to Operating System Commands                     *)
(*                                                             *)
(*  CMSX and WINX                                              *)
(*                                                             *)
(***************************************************************)




procedure CMSX ( CMD : CHARPTR ; var RETCODE : INTEGER ) ;

   const CMDEND = '#' ;

   var CMSCMD : CHAR128 ;
       CP : CHARPTR ;
       CPT : CHARPTR ;
       TOK : TOKEN ;
       I : INTEGER ;

   begin (* CMSX *)
     CP := CMD ;
     CPT := ADDR ( CMSCMD ) ;
     CMSCMD := ' ' ;
     repeat

     /*********************/
     /* blanks ueberlesen */
     /*********************/

       while ( CP -> = ' ' ) and ( CP -> <> CMDEND ) and ( CP -> <> CHR
       ( 0 ) ) do
         CP := PTRADD ( CP , 1 ) ;

     /**********************************/
     /* wenn nichts mehr da, dann raus */
     /**********************************/

       if ( CP -> = CMDEND ) or ( CP -> = CHR ( 0 ) ) then
         break ;

     /******************************************/
     /* wenn klammer auf, dann separates token */
     /******************************************/

       if CP -> = '(' then
         begin
           TOK := '(' ;
           CP := PTRADD ( CP , 1 ) ;
         end (* then *)

     /*************************************************/
     /* andernfalls token, bis trennzeichen erscheint */
     /*************************************************/

       else
         begin
           I := 0 ;
           TOK := ' ' ;
           while ( CP -> <> ' ' ) and ( CP -> <> '(' ) and ( CP -> <>
           CMDEND ) and ( CP -> <> CHR ( 0 ) ) do
             begin
               if I < SIZEOF ( TOKEN ) then
                 begin
                   I := I + 1 ;
                   TOK [ I ] := CP -> ;
                 end (* then *) ;
               CP := PTRADD ( CP , 1 ) ;
             end (* while *)
         end (* else *) ;

     /************************************************/
     /* token in cmscmd uebertragen, wenn noch platz */
     /************************************************/

       if PTRDIFF ( CPT , ADDR ( CMSCMD ) ) < SIZEOF ( CMSCMD ) - 8
       then
         begin
           MEMCPY ( CPT , ADDR ( TOK ) , SIZEOF ( TOKEN ) ) ;
           CPT := PTRADD ( CPT , SIZEOF ( TOKEN ) ) ;
         end (* then *)
     until FALSE ;

     /************************************************/
     /* Ende-Kennung dahinter                        */
     /************************************************/

     MEMSET ( CPT , CHR ( 255 ) , 8 ) ;

     /************************************************/
     /* CMS-Kommando aufrufen via $PASSYS / 11       */
     /************************************************/

     if FALSE then
       WRITELN ( 'test cmsx: <' , CMSCMD , '>' ) ;
     RETCODE := PTR2INT ( $PASSYS ( 11 , ADDR ( CMSCMD ) ) ) ;
     if FALSE then
       WRITELN ( 'test cmsx: retcode = ' , RETCODE ) ;
   end (* CMSX *) ;



procedure WINX ( CMD : CHARPTR ; var RETCODE : INTEGER ) ;

   const CMDEND = '#' ;

   var CMSCMD : CHAR128 ;
       CP : CHARPTR ;
       CPSTART : CHARPTR ;
       ILEN : INTEGER ;

   begin (* WINX *)
     CP := CMD ;
     CMSCMD := ' ' ;

     /*********************/
     /* blanks ueberlesen */
     /*********************/

     while ( CP -> = ' ' ) and ( CP -> <> CMDEND ) and ( CP -> <> CHR (
     0 ) ) do
       CP := PTRADD ( CP , 1 ) ;
     CPSTART := CP ;
     while ( CP -> <> CMDEND ) and ( CP -> <> CHR ( 0 ) ) do
       CP := PTRADD ( CP , 1 ) ;
     ILEN := PTRDIFF ( CP , CPSTART ) ;
     if ILEN > SIZEOF ( CMSCMD ) - 1 then
       ILEN := SIZEOF ( CMSCMD ) - 1 ;
     MEMCPY ( ADDR ( CMSCMD ) , CPSTART , ILEN ) ;
     CMSCMD [ ILEN ] := CHR ( 0 ) ;

     /************************************************/
     /* CMS-Kommando aufrufen via $PASSYS / 12       */
     /************************************************/

     if FALSE then
       WRITELN ( 'test winx: <' , CMSCMD , '>' ) ;
     RETCODE := PTR2INT ( $PASSYS ( 12 , ADDR ( CMSCMD ) ) ) ;
     if FALSE then
       WRITELN ( 'test winx: retcode = ' , RETCODE ) ;
   end (* WINX *) ;





(***************************************************************)
(*$D-,A+                                                       *)
(***************************************************************)
(*                                                             *)
(*  String-Handling Funktionen fuer Pascal                     *)
(*                                                             *)
(*  11.2016 - bernd.oppolzer@yahoo.com                         *)
(*                                                             *)
(***************************************************************)
(*                                                             *)
(*  see the table in the Pascal compiler Pass 1 (PASCAL1);     *)
(*  this table maps the function names to the procedure        *)
(*  entries and subfunction numbers here                       *)
(*                                                             *)
(*  'SUBSTR   ' , 86 , FUNC , '$PASSTR1' , 1 , 3 , 'V' , 1     *)
(*  'DELETE   ' , 87 , FUNC , '$PASSTR1' , 2 , 3 , 'V' , 1     *)
(*  'RTRIM    ' , 88 , FUNC , '$PASSTR1' , 3 , 1 , 'V' , 1     *)
(*  'LTRIM    ' , 89 , FUNC , '$PASSTR1' , 4 , 1 , 'V' , 1     *)
(*  'TRIM     ' , 90 , FUNC , '$PASSTR1' , 5 , 1 , 'V' , 1     *)
(*  'COMPRESS ' , 91 , FUNC , '$PASSTR1' , 6 , 1 , 'V' , 1     *)
(*  'INDEX    ' , 93 , FUNC , '$PASSTR2' , 1 , 2 , 'I' , 2     *)
(*  'VERIFY   ' , 94 , FUNC , '$PASSTR2' , 2 , 2 , 'I' , 2     *)
(*  'TRANSLATE' , 95 , FUNC , '$PASSTR3' , 1 , 3 , 'V' , 3     *)
(*  'LASTINDEX' , 99 , FUNC , '$PASSTR2' , 3 , 2 , 'I' , 2     *)
(*  'LEFT    ' , 100 , FUNC , '$PASSTR1' , 7 , 2 , 'V' , 1     *)
(*  'RIGHT   ' , 101 , FUNC , '$PASSTR1' , 8 , 2 , 'V' , 1     *)
(*                                                             *)
(***************************************************************)




function $PASSTR1 ( FUNCCODE : INTEGER ; const S1 : STRING ; I1 :
                  INTEGER ; I2 : INTEGER ) : STRING ;

   type LENGTHF = 0 .. 32767 ;

   var LEN : INTEGER ;
       START : INTEGER ;
       RESTL : INTEGER ;
       LS1 : INTEGER ;
       I : INTEGER ;
       X : INTEGER ;
       P : ANYPTR ;
       Q : ANYPTR ;
       LF : -> LENGTHF ;
       CP : -> CHAR ;
       CP2 : -> CHAR ;
       C : CHAR ;
       BLANK : INTEGER ;

   begin (* $PASSTR1 *)
     case FUNCCODE of

     /*********************************/
     /* SUBSTR                        */
     /*********************************/

       1 : begin
             START := I1 ;
             LEN := I2 ;
             if START < 1 then
               $ERROR ( 1201 ) ;
             if LEN < 0 then
               begin
                 if START > LENGTH ( S1 ) then
                   $ERROR ( 1201 ) ;
                 LEN := LENGTH ( S1 ) - START + 1 ;
               end (* then *)
             else
               begin
                 X := START + LEN - 1 ;
                 if X > LENGTH ( S1 ) then
                   $ERROR ( 1201 ) ;
               end (* else *) ;
             $PASSTR1 := REPEATSTR ( ' ' , LEN ) ;
             P := STRRESULTP ;
             Q := ADDR ( S1 [ START ] ) ;
             MEMCPY ( P , Q , LEN ) ;
           end (* tag/ca *) ;

     /*********************************/
     /* DELETE                        */
     /*********************************/

       2 : begin
             START := I1 ;
             LEN := I2 ;
             if START < 1 then
               $ERROR ( 1201 ) ;
             if LEN < 0 then
               begin
                 if START > LENGTH ( S1 ) then
                   $ERROR ( 1201 ) ;
                 RESTL := START - 1 ;
                 $PASSTR1 := REPEATSTR ( ' ' , RESTL ) ;
                 Q := ADDR ( S1 [ 1 ] ) ;
                 MEMCPY ( STRRESULTP , Q , RESTL ) ;
               end (* then *)
             else
               begin
                 LS1 := LENGTH ( S1 ) ;
                 X := START + LEN - 1 ;
                 if X > LS1 then
                   $ERROR ( 1201 ) ;
                 RESTL := LS1 - X ;
                 $PASSTR1 := REPEATSTR ( ' ' , LS1 - LEN ) ;
                 if START > 1 then
                   begin
                     Q := ADDR ( S1 [ 1 ] ) ;
                     MEMCPY ( STRRESULTP , Q , START - 1 ) ;
                   end (* then *) ;
                 if RESTL > 0 then
                   begin
                     P := PTRADD ( STRRESULTP , START - 1 ) ;
                     Q := ADDR ( S1 [ START + LEN ] ) ;
                     MEMCPY ( P , Q , RESTL ) ;
                   end (* then *)
               end (* else *) ;
           end (* tag/ca *) ;

     /*********************************/
     /* RTRIM                         */
     /*********************************/

       3 : begin
             LS1 := LENGTH ( S1 ) ;
             if LS1 = 0 then
               begin
                 $PASSTR1 := S1 ;
                 return ;
               end (* then *) ;
             CP := ADDR ( S1 [ LS1 ] ) ;
             LEN := LS1 ;
             while LEN > 0 do
               begin
                 if CP -> <> ' ' then
                   break ;
                 CP := PTRADD ( CP , - 1 ) ;
                 LEN := LEN - 1 ;
               end (* while *) ;
             $PASSTR1 := REPEATSTR ( ' ' , LEN ) ;
             Q := ADDR ( S1 [ 1 ] ) ;
             MEMCPY ( STRRESULTP , Q , LEN ) ;
           end (* tag/ca *) ;

     /*********************************/
     /* LTRIM                         */
     /*********************************/

       4 : begin
             LS1 := LENGTH ( S1 ) ;
             if LS1 = 0 then
               begin
                 $PASSTR1 := S1 ;
                 return ;
               end (* then *) ;
             LEN := LS1 ;
             CP := ADDR ( S1 [ 1 ] ) ;
             while LEN > 0 do
               begin
                 if CP -> <> ' ' then
                   break ;
                 CP := PTRADD ( CP , 1 ) ;
                 LEN := LEN - 1 ;
               end (* while *) ;
             $PASSTR1 := REPEATSTR ( ' ' , LEN ) ;
             MEMCPY ( STRRESULTP , CP , LEN ) ;
           end (* tag/ca *) ;

     /*********************************/
     /* TRIM                          */
     /*********************************/

       5 : begin
             LS1 := LENGTH ( S1 ) ;
             if LS1 = 0 then
               begin
                 $PASSTR1 := S1 ;
                 return ;
               end (* then *) ;
             CP := ADDR ( S1 [ LS1 ] ) ;
             LEN := LS1 ;
             while LEN > 0 do
               begin
                 if CP -> <> ' ' then
                   break ;
                 CP := PTRADD ( CP , - 1 ) ;
                 LEN := LEN - 1 ;
               end (* while *) ;
             CP := ADDR ( S1 [ 1 ] ) ;
             while LEN > 0 do
               begin
                 if CP -> <> ' ' then
                   break ;
                 CP := PTRADD ( CP , 1 ) ;
                 LEN := LEN - 1 ;
               end (* while *) ;
             $PASSTR1 := REPEATSTR ( ' ' , LEN ) ;
             MEMCPY ( STRRESULTP , CP , LEN ) ;
           end (* tag/ca *) ;

     /*********************************/
     /* COMPRESS                      */
     /*********************************/

       6 : begin
             LS1 := LENGTH ( S1 ) ;
             if LS1 = 0 then
               begin
                 $PASSTR1 := S1 ;
                 return ;
               end (* then *) ;
             $PASSTR1 := REPEATSTR ( ' ' , LS1 ) ;
             CP := ADDR ( S1 [ 1 ] ) ;
             CP2 := STRRESULTP ;
             BLANK := 0 ;
             LEN := 0 ;
             for I := 1 to LS1 do
               begin
                 C := CP -> ;
                 if C = ' ' then
                   BLANK := BLANK + 1
                 else
                   BLANK := 0 ;
                 if BLANK <= 1 then
                   begin
                     LEN := LEN + 1 ;
                     CP2 -> := C ;
                     CP2 := PTRADD ( CP2 , 1 ) ;
                   end (* then *) ;
                 CP := PTRADD ( CP , 1 ) ;
               end (* for *) ;
             LF := RESULTP ;
             LF := PTRADD ( LF , SIZEOF ( LENGTHF ) ) ;
             LF -> := LEN ;
           end (* tag/ca *) ;

     /*********************************/
     /* LEFT                          */
     /*********************************/

       7 : begin
             START := 1 ;
             LEN := I1 ;
             if LEN < 1 then
               $ERROR ( 1201 ) ;
             LS1 := LENGTH ( S1 ) ;
             if LS1 > LEN then
               LS1 := LEN ;
             $PASSTR1 := REPEATSTR ( ' ' , LEN ) ;
             if LS1 > 0 then
               begin
                 P := STRRESULTP ;
                 Q := ADDR ( S1 [ START ] ) ;
                 MEMCPY ( P , Q , LS1 )
               end (* then *)
           end (* tag/ca *) ;

     /*********************************/
     /* RIGHT                         */
     /*********************************/

       8 : begin
             START := 1 ;
             LEN := I1 ;
             if LEN < 1 then
               $ERROR ( 1201 ) ;
             LS1 := LENGTH ( S1 ) ;
             if LS1 > LEN then
               begin
                 START := LS1 - LEN + 1 ;
                 LS1 := LEN ;
                 X := 0 ;
               end (* then *)
             else
               begin
                 START := 1 ;
                 X := LEN - LS1 ;
               end (* else *) ;
             $PASSTR1 := REPEATSTR ( ' ' , LEN ) ;
             if LS1 > 0 then
               begin
                 P := STRRESULTP ;
                 if X > 0 then
                   P := PTRADD ( P , X ) ;
                 Q := ADDR ( S1 [ START ] ) ;
                 MEMCPY ( P , Q , LS1 )
               end (* then *)
           end (* tag/ca *) ;

     /*********************************/
     /* unknown subfunction           */
     /*********************************/

       otherwise
         $ERROR ( 1120 ) ;
     end (* case *) ;
   end (* $PASSTR1 *) ;



function $PASSTR2 ( FUNCCODE : INTEGER ; const S1 : STRING ; const S2 :
                  STRING ) : INTEGER ;

   var LS1 : INTEGER ;
       LS2 : INTEGER ;
       CP1 : -> CHAR ;
       CP1START : -> CHAR ;
       CP1LIMIT : -> CHAR ;
       CP1FOUND2 : -> CHAR ;
       CP2 : -> CHAR ;
       CP2START : -> CHAR ;
       LFOUND : INTEGER ;
       I : INTEGER ;
       OKSET : set of CHAR ;
       STATUS : INTEGER ;
       C2A : CHAR ;
       C2E : CHAR ;
       CP1F : -> CHAR ;
       CP1T : -> CHAR ;

   begin (* $PASSTR2 *)
     case FUNCCODE of

     /*********************************/
     /* INDEX                         */
     /*********************************/

       1 : begin

     //***************************************
     // ergebnis zuerst mal auf Null          
     // laengen besorgen                      
     // wenn laenge des suchstrings groesser  
     // kann nichts gefunden werden           
     //***************************************

             $PASSTR2 := 0 ;
             LS1 := LENGTH ( S1 ) ;
             LS2 := LENGTH ( S2 ) ;
             if LS2 > LS1 then
               return ;

     //**************************************************
     // spezialvariante fuer laenge des suchstrings = 1  
     // dann ist einiges einfacher                       
     //**************************************************

             if LS2 = 1 then
               begin
                 CP1 := ADDR ( S1 [ 1 ] ) ;
                 CP1START := CP1 ;
                 C2A := S2 [ 1 ] ;
                 LFOUND := 0 ;
                 CP1LIMIT := PTRADD ( CP1 , LS1 ) ;
                 while PTRDIFF ( CP1LIMIT , CP1 ) > 0 do
                   if CP1 -> <> C2A then
                     CP1 := PTRADD ( CP1 , 1 )
                   else
                     begin
                       LFOUND := 1 ;
                       break ;
                     end (* else *) ;
                 if LFOUND = 1 then
                   $PASSTR2 := PTRDIFF ( CP1 , CP1START ) + 1 ;
               end (* then *)

     //****************************************************
     // optimierung: zuerst wird nach anfang und ende des  
     // suchstrings gesucht                                
     //****************************************************

             else
               begin
                 CP1 := ADDR ( S1 [ 1 ] ) ;
                 CP1START := CP1 ;
                 CP2START := ADDR ( S2 [ 1 ] ) ;
                 C2A := S2 [ 1 ] ;
                 C2E := S2 [ LS2 ] ;
                 LFOUND := 0 ;
                 CP1LIMIT := PTRADD ( CP1 , LS1 ) ;
                 CP1FOUND2 := NIL ;
                 STATUS := 0 ;
                 while PTRDIFF ( CP1LIMIT , CP1 ) > 0 do
                   case STATUS of
                     0 : begin
                           if CP1 -> <> C2A then
                             CP1 := PTRADD ( CP1 , 1 )
                           else
                             STATUS := 1
                         end (* tag/ca *) ;
                     1 : begin
                           CP1T := PTRADD ( CP1 , LS2 - 1 ) ;
                           if CP1T -> <> C2E then
                             begin
                               CP1 := PTRADD ( CP1 , 1 ) ;
                               STATUS := 0 ;
                               continue ;
                             end (* then *) ;
                           CP1FOUND2 := NIL ;
                           LFOUND := 1 ;
                           if LFOUND >= LS2 then
                             break ;
                           CP1 := PTRADD ( CP1 , 1 ) ;
                           CP2 := PTRADD ( CP2START , 1 ) ;
                           STATUS := 2 ;
                         end (* tag/ca *) ;
                     2 : begin
                           if CP1 -> <> CP2 -> then
                             begin
                               if CP1FOUND2 <> NIL then
                                 CP1 := CP1FOUND2 ;
                               LFOUND := 0 ;
                               STATUS := 0 ;
                               continue ;
                             end (* then *) ;
                           LFOUND := LFOUND + 1 ;
                           if LFOUND >= LS2 then
                             break ;
                           if CP1 -> = CP2START -> then
                             CP1FOUND2 := CP1 ;
                           CP1 := PTRADD ( CP1 , 1 ) ;
                           CP2 := PTRADD ( CP2 , 1 ) ;
                         end (* tag/ca *) ;
                   end (* case *) ;
                 if LFOUND >= LS2 then
                   $PASSTR2 := PTRDIFF ( CP1 , CP1START ) - LS2 + 2 ;
               end (* else *)
           end (* tag/ca *) ;

     /*********************************/
     /* VERIFY                        */
     /*********************************/

       2 : begin
             OKSET := [ ] ;
             for I := 1 to LENGTH ( S2 ) do
               OKSET := OKSET + [ S2 [ I ] ] ;
             $PASSTR2 := 0 ;
             for I := 1 to LENGTH ( S1 ) do
               if not ( S1 [ I ] in OKSET ) then
                 begin
                   $PASSTR2 := I ;
                   break ;
                 end (* then *) ;
           end (* tag/ca *) ;

     /*********************************/
     /* LastIndex                     */
     /*********************************/

       3 : begin

     //***************************************
     // ergebnis zuerst mal auf Null          
     // laengen besorgen                      
     // wenn laenge des suchstrings groesser  
     // kann nichts gefunden werden           
     //***************************************

             $PASSTR2 := 0 ;
             LS1 := LENGTH ( S1 ) ;
             LS2 := LENGTH ( S2 ) ;
             if LS2 > LS1 then
               return ;

     //**************************************************
     // spezialvariante fuer laenge des suchstrings = 1  
     // dann ist einiges einfacher                       
     //**************************************************

             if LS2 = 1 then
               begin
                 CP1 := ADDR ( S1 [ LS1 ] ) ;
                 CP1START := ADDR ( S1 [ 1 ] ) ;
                 CP1LIMIT := CP1START ;
                 C2A := S2 [ 1 ] ;
                 LFOUND := 0 ;
                 while PTRDIFF ( CP1 , CP1LIMIT ) >= 0 do
                   if CP1 -> <> C2A then
                     CP1 := PTRADD ( CP1 , - 1 )
                   else
                     begin
                       LFOUND := 1 ;
                       break ;
                     end (* else *) ;
                 if LFOUND = 1 then
                   $PASSTR2 := PTRDIFF ( CP1 , CP1START ) + 1 ;
               end (* then *)

     //****************************************************
     // optimierung: zuerst wird nach anfang und ende des  
     // suchstrings gesucht                                
     //****************************************************

             else
               begin
                 CP1 := ADDR ( S1 [ LS1 - LS2 + 1 ] ) ;
                 CP1START := ADDR ( S1 [ 1 ] ) ;
                 CP1LIMIT := CP1START ;
                 CP2START := ADDR ( S2 [ 1 ] ) ;
                 C2A := S2 [ 1 ] ;
                 C2E := S2 [ LS2 ] ;
                 LFOUND := 0 ;
                 CP1FOUND2 := NIL ;
                 STATUS := 0 ;
                 while PTRDIFF ( CP1 , CP1LIMIT ) >= 0 do
                   case STATUS of
                     0 : begin
                           if CP1 -> <> C2A then
                             CP1 := PTRADD ( CP1 , - 1 )
                           else
                             STATUS := 1
                         end (* tag/ca *) ;
                     1 : begin
                           CP1F := CP1 ;
                           CP1T := PTRADD ( CP1 , LS2 - 1 ) ;
                           if CP1T -> <> C2E then
                             begin
                               CP1 := PTRADD ( CP1 , - 1 ) ;
                               STATUS := 0 ;
                               continue ;
                             end (* then *) ;
                           LFOUND := 1 ;
                           if LFOUND >= LS2 then
                             break ;
                           CP1 := PTRADD ( CP1 , 1 ) ;
                           CP2 := PTRADD ( CP2START , 1 ) ;
                           STATUS := 2 ;
                         end (* tag/ca *) ;
                     2 : begin
                           if CP1 -> <> CP2 -> then
                             begin
                               CP1 := PTRADD ( CP1F , - 1 ) ;
                               LFOUND := 0 ;
                               STATUS := 0 ;
                               continue ;
                             end (* then *) ;
                           LFOUND := LFOUND + 1 ;
                           if LFOUND >= LS2 then
                             break ;
                           CP1 := PTRADD ( CP1 , 1 ) ;
                           CP2 := PTRADD ( CP2 , 1 ) ;
                         end (* tag/ca *) ;
                   end (* case *) ;
                 if LFOUND >= LS2 then
                   $PASSTR2 := PTRDIFF ( CP1 , CP1START ) - LS2 + 2 ;
               end (* else *)
           end (* tag/ca *) ;

     /*********************************/
     /* unknown subfunction           */
     /*********************************/

       otherwise
         $ERROR ( 1120 ) ;
     end (* case *) ;
   end (* $PASSTR2 *) ;



function $PASSTR3 ( FUNCCODE : INTEGER ; const S1 : STRING ; const S2 :
                  STRING ; const S3 : STRING ) : STRING ;

   type TRANSLATE_TAB = array [ 0 .. 255 ] of CHAR ;

   var TTAB : TRANSLATE_TAB ;
       TTAB2 : array [ 1 .. 128 ] of CHAR ;
       L1 , L2 , L3 : INTEGER ;
       I : INTEGER ;
       CP : -> CHAR ;

   const IDENT_TAB : TRANSLATE_TAB =
         ( X'00' , X'01' , X'02' , X'03' , X'04' , X'05' , X'06' ,
           X'07' , X'08' , X'09' , X'0a' , X'0b' , X'0c' , X'0d' ,
           X'0e' , X'0f' , X'10' , X'11' , X'12' , X'13' , X'14' ,
           X'15' , X'16' , X'17' , X'18' , X'19' , X'1a' , X'1b' ,
           X'1c' , X'1d' , X'1e' , X'1f' , X'20' , X'21' , X'22' ,
           X'23' , X'24' , X'25' , X'26' , X'27' , X'28' , X'29' ,
           X'2a' , X'2b' , X'2c' , X'2d' , X'2e' , X'2f' , X'30' ,
           X'31' , X'32' , X'33' , X'34' , X'35' , X'36' , X'37' ,
           X'38' , X'39' , X'3a' , X'3b' , X'3c' , X'3d' , X'3e' ,
           X'3f' , X'40' , X'41' , X'42' , X'43' , X'44' , X'45' ,
           X'46' , X'47' , X'48' , X'49' , X'4a' , X'4b' , X'4c' ,
           X'4d' , X'4e' , X'4f' , X'50' , X'51' , X'52' , X'53' ,
           X'54' , X'55' , X'56' , X'57' , X'58' , X'59' , X'5a' ,
           X'5b' , X'5c' , X'5d' , X'5e' , X'5f' , X'60' , X'61' ,
           X'62' , X'63' , X'64' , X'65' , X'66' , X'67' , X'68' ,
           X'69' , X'6a' , X'6b' , X'6c' , X'6d' , X'6e' , X'6f' ,
           X'70' , X'71' , X'72' , X'73' , X'74' , X'75' , X'76' ,
           X'77' , X'78' , X'79' , X'7a' , X'7b' , X'7c' , X'7d' ,
           X'7e' , X'7f' , X'80' , X'81' , X'82' , X'83' , X'84' ,
           X'85' , X'86' , X'87' , X'88' , X'89' , X'8a' , X'8b' ,
           X'8c' , X'8d' , X'8e' , X'8f' , X'90' , X'91' , X'92' ,
           X'93' , X'94' , X'95' , X'96' , X'97' , X'98' , X'99' ,
           X'9a' , X'9b' , X'9c' , X'9d' , X'9e' , X'9f' , X'a0' ,
           X'a1' , X'a2' , X'a3' , X'a4' , X'a5' , X'a6' , X'a7' ,
           X'a8' , X'a9' , X'aa' , X'ab' , X'ac' , X'ad' , X'ae' ,
           X'af' , X'b0' , X'b1' , X'b2' , X'b3' , X'b4' , X'b5' ,
           X'b6' , X'b7' , X'b8' , X'b9' , X'ba' , X'bb' , X'bc' ,
           X'bd' , X'be' , X'bf' , X'c0' , X'c1' , X'c2' , X'c3' ,
           X'c4' , X'c5' , X'c6' , X'c7' , X'c8' , X'c9' , X'ca' ,
           X'cb' , X'cc' , X'cd' , X'ce' , X'cf' , X'd0' , X'd1' ,
           X'd2' , X'd3' , X'd4' , X'd5' , X'd6' , X'd7' , X'd8' ,
           X'd9' , X'da' , X'db' , X'dc' , X'dd' , X'de' , X'df' ,
           X'e0' , X'e1' , X'e2' , X'e3' , X'e4' , X'e5' , X'e6' ,
           X'e7' , X'e8' , X'e9' , X'ea' , X'eb' , X'ec' , X'ed' ,
           X'ee' , X'ef' , X'f0' , X'f1' , X'f2' , X'f3' , X'f4' ,
           X'f5' , X'f6' , X'f7' , X'f8' , X'f9' , X'fa' , X'fb' ,
           X'fc' , X'fd' , X'fe' , X'ff' ) ;

   begin (* $PASSTR3 *)
     case FUNCCODE of

     /*********************************/
     /* TRANSLATE                     */
     /*********************************/

       1 : begin
             L2 := LENGTH ( S2 ) ;
             L3 := LENGTH ( S3 ) ;
             if L3 <> 0 then
               begin
                 if L3 <> L2 then
                   $ERROR ( 1202 ) ;
                 TTAB := IDENT_TAB ;
                 for I := 1 to L2 do
                   TTAB [ ORD ( S3 [ I ] ) ] := S2 [ I ] ;
               end (* then *)
             else
               begin
                 if L2 <> 256 then
                   $ERROR ( 1202 ) ;
                 if FALSE then
                   for I := 1 to L2 do
                     WRITELN ( 's2-test: ' , ORD ( S2 [ I ] ) ) ;
                 TTAB2 := SUBSTR ( S2 , 1 , 128 ) ;
                 MEMCPY ( ADDR ( TTAB ) , ADDR ( TTAB2 ) , 128 ) ;
                 TTAB2 := SUBSTR ( S2 , 129 , 128 ) ;
                 MEMCPY ( ADDR ( TTAB [ 128 ] ) , ADDR ( TTAB2 ) , 128
                          ) ;
                 if FALSE then
                   for I := 0 to 255 do
                     WRITELN ( 'ttab-test: ' , ORD ( TTAB [ I ] ) ) ;
               end (* else *) ;
             $PASSTR3 := S1 ;
             L1 := LENGTH ( S1 ) ;
             CP := STRRESULTP ;
             for I := 1 to L1 do
               begin
                 CP -> := TTAB [ ORD ( CP -> ) ] ;
                 CP := PTRADD ( CP , 1 ) ;
               end (* for *) ;
           end (* tag/ca *) ;

     /*********************************/
     /* unknown subfunction           */
     /*********************************/

       otherwise
         $ERROR ( 1120 ) ;
     end (* case *) ;
   end (* $PASSTR3 *) ;



function FILESTAT ( var X : ANYFILE ) : CHAR ;

   var FCB : VOIDPTR ;
       CPS : -> CHAR ;

   begin (* FILESTAT *)
     if PLATF = PLATF_UNKNOWN then
       CHECK_PLATFORM ;
     if PLATF = PLATF_INTEL then
       begin
         FCB := FILEFCB ( X ) ;
         CPS := PTRADD ( FCB , 278 ) ;
       end (* then *)
     else
       begin
         FCB := FILEFCB ( X ) ;
         CPS := PTRADD ( FCB , 32 ) ;
       end (* else *) ;
     FILESTAT := CPS -> ;
   end (* FILESTAT *) ;



local function TOUPPER ( CH : CHAR ) : CHAR ;

(******************************************)
(*   SETZT KLEINBUCHSTABEN IN GROSSE UM   *)
(******************************************)


   begin (* TOUPPER *)
     if CH in KLEINBUCHST then
       TOUPPER := CHR ( ORD ( CH ) - ORD ( 'a' ) + ORD ( 'A' ) )
     else
       TOUPPER := CH
   end (* TOUPPER *) ;



function $PASRDR ( var F : TEXT ; WIDTH : INTEGER ) : REAL ;

//**********************************************************
// function to read reals from files                        
// rewritten in Pascal - portable solution in 2020          
// supports width parameter like in Pascal/VS               
//**********************************************************
// Bernd Oppolzer - New Stanford Pascal                     
//**********************************************************


   var SIGN : INTEGER ;
       RETVAL : REAL ;
       RETVALI : INTEGER ;
       SCALE : REAL ;
       LEADINGZ : BOOLEAN ;
       DIGITS : INTEGER ;
       BEFOREDP : BOOLEAN ;
       FORMATTED : BOOLEAN ;
       EXPO : BOOLEAN ;
       SCALEXP : REAL ;
       EXPVALI : INTEGER ;
       SIGNEXP : INTEGER ;

   const SCALTAB_PLUS : array [ 1 .. 10 ] of REAL =
         ( 10.0 , 100.0 , 1000.0 , 10000.0 , 100000.0 , 1000000.0 ,
           10000000.0 , 100000000.0 , 1000000000.0 , 10000000000.0 ) ;
         SCALTAB_MINUS : array [ 1 .. 10 ] of REAL =
         ( 0.1 , 0.01 , 0.001 , 0.0001 , 0.00001 , 0.000001 , 0.0000001
           , 0.00000001 , 0.000000001 , 0.0000000001 ) ;

   begin (* $PASRDR *)
     if WIDTH = 0 then
       begin
         $PASRDR := 0.0 ;
         return
       end (* then *) ;
     FORMATTED := TRUE ;
     if WIDTH < 0 then
       begin
         FORMATTED := FALSE ;
         WIDTH := 999999999 ;
       end (* then *) ;
     if FILESTAT ( F ) = '0' then
       RESET ( F ) ;
     if EOLN ( F ) then
       GET ( F ) ;
     if EOF ( F ) then
       begin
         $PASRDR := 0.0 ;
         return
       end (* then *) ;
     SIGN := 1 ;
     RETVAL := 0.0 ;
     RETVALI := 0 ;

     //**********************
     // skip leading blanks  
     //**********************

     while ( F -> = ' ' ) and ( WIDTH > 0 ) do
       begin
         if FORMATTED then
           begin
             GET ( F ) ;
             WIDTH := WIDTH - 1 ;
             if EOLN ( F ) or EOF ( F ) or ( WIDTH <= 0 ) then
               $ERROR ( 1204 ) ;
           end (* then *)
         else
           begin
             GET ( F ) ;
             if EOF ( F ) then
               begin
                 $PASRDR := RETVAL ;
                 return
               end (* then *)
           end (* else *)
       end (* while *) ;

     //********************************************************
     // leadingz = reading and ignoring leading zeroes         
     // digits = number of digits (if < 10 then integer math)  
     // beforedp = decimal point not yet found                 
     // scale = compute scale factor from dec point            
     //********************************************************

     LEADINGZ := TRUE ;
     DIGITS := 0 ;
     BEFOREDP := TRUE ;
     SCALE := 1.0 ;
     EXPO := FALSE ;
     case F -> of
       '-' : begin
               SIGN := - 1 ;
               GET ( F ) ;
               WIDTH := WIDTH - 1 ;
               if WIDTH <= 0 then
                 $ERROR ( 1204 ) ;
               if not ( F -> in [ '0' .. '9' ] ) then
                 $ERROR ( 1204 ) ;
             end (* tag/ca *) ;
       '+' : begin
               GET ( F ) ;
               WIDTH := WIDTH - 1 ;
               if WIDTH <= 0 then
                 $ERROR ( 1204 ) ;
               if not ( F -> in [ '0' .. '9' ] ) then
                 $ERROR ( 1204 ) ;
             end (* tag/ca *) ;
       '.' : begin
               GET ( F ) ;
               WIDTH := WIDTH - 1 ;
               BEFOREDP := FALSE ;
             end (* tag/ca *) ;
       '0' .. '9' :
         begin
           RETVALI := ORD ( F -> ) - ORD ( '0' ) ;
           LEADINGZ := RETVALI = 0 ;
           if not LEADINGZ then
             DIGITS := 1 ;
           GET ( F ) ;
           WIDTH := WIDTH - 1 ;
         end (* tag/ca *) ;
       otherwise
         $ERROR ( 1204 )
     end (* case *) ;
     if EOLN ( F ) or ( WIDTH <= 0 ) then
       begin
         RETVAL := RETVALI ;
         $PASRDR := RETVAL ;
         return
       end (* then *) ;

     //*************************************************
     // main loop for mantissa                          
     // logic controlled by leadingz, digits, beforedp  
     // final results in retval and scale               
     //*************************************************

     while WIDTH > 0 do
       begin
         case F -> of
           '.' : begin
                   if not BEFOREDP then
                     $ERROR ( 1205 ) ;
                   GET ( F ) ;
                   WIDTH := WIDTH - 1 ;
                   BEFOREDP := FALSE ;
                 end (* tag/ca *) ;
           'e' , 'E' :
             begin
               GET ( F ) ;
               WIDTH := WIDTH - 1 ;
               EXPO := TRUE ;
               break ;
             end (* tag/ca *) ;
           '0' .. '9' :
             begin
               if not BEFOREDP then
                 SCALE := SCALE * 10.0 ;
               if F -> <> '0' then
                 LEADINGZ := FALSE ;
               if not LEADINGZ then
                 begin
                   DIGITS := DIGITS + 1 ;
                   if DIGITS < 10 then
                     begin
                       RETVALI := RETVALI * 10 + ( ORD ( F -> ) - ORD (
                                  '0' ) ) ;
                       if DIGITS = 9 then
                         RETVAL := RETVALI ;
                     end (* then *)
                   else
                     begin
                       RETVAL := RETVAL * 10 + ( ORD ( F -> ) - ORD (
                                 '0' ) ) ;
                     end (* else *) ;
                 end (* then *) ;
               GET ( F ) ;
               WIDTH := WIDTH - 1 ;
             end (* tag/ca *) ;
           otherwise
             break
         end (* case *) ;
       end (* while *) ;

     //*******************************************************
     // if real value fields have not been set, set them now  
     //*******************************************************

     if DIGITS < 9 then
       RETVAL := RETVALI ;

     //*******************************
     // read exponent if expo = true  
     // and compose final result      
     //*******************************

     SCALEXP := 1.0 ;
     if EXPO then
       begin

     //*******************************
     // E has already been found      
     // exponent may have sign        
     //*******************************

         SIGNEXP := 1 ;
         EXPVALI := 0 ;
         case F -> of
           '-' : begin
                   SIGNEXP := - 1 ;
                   GET ( F ) ;
                   WIDTH := WIDTH - 1 ;
                   if WIDTH <= 0 then
                     $ERROR ( 1206 ) ;
                   if not ( F -> in [ '0' .. '9' ] ) then
                     $ERROR ( 1206 ) ;
                 end (* tag/ca *) ;
           '+' : begin
                   GET ( F ) ;
                   WIDTH := WIDTH - 1 ;
                   if WIDTH <= 0 then
                     $ERROR ( 1206 ) ;
                   if not ( F -> in [ '0' .. '9' ] ) then
                     $ERROR ( 1206 ) ;
                 end (* tag/ca *) ;
           '0' .. '9' :
             begin
               EXPVALI := ORD ( F -> ) - ORD ( '0' ) ;
               GET ( F ) ;
               WIDTH := WIDTH - 1 ;
             end (* tag/ca *) ;
           otherwise
             $ERROR ( 1206 )
         end (* case *) ;
         while ( F -> in [ '0' .. '9' ] ) and ( WIDTH > 0 ) do
           begin
             EXPVALI := EXPVALI * 10 + ( ORD ( F -> ) - ORD ( '0' ) ) ;
             GET ( F ) ;
             WIDTH := WIDTH - 1 ;
           end (* while *) ;

     //*******************************************
     // compute scalexp depending on exponent     
     //*******************************************

         if SIGNEXP = - 1 then
           begin
             while EXPVALI > 10 do
               begin
                 SCALEXP := SCALEXP * SCALTAB_MINUS [ 10 ] ;
                 EXPVALI := EXPVALI - 10
               end (* while *) ;
             SCALEXP := SCALEXP * SCALTAB_MINUS [ EXPVALI ] ;
           end (* then *)
         else
           begin
             while EXPVALI > 10 do
               begin
                 SCALEXP := SCALEXP * SCALTAB_PLUS [ 10 ] ;
                 EXPVALI := EXPVALI - 10
               end (* while *) ;
             SCALEXP := SCALEXP * SCALTAB_PLUS [ EXPVALI ] ;
           end (* else *) ;

     //*******************************************
     // compose final result with scalexp         
     //*******************************************

         $PASRDR := RETVAL / SCALE * SCALEXP * SIGN ;
       end (* then *)
     else
       $PASRDR := RETVAL / SCALE * SIGN ;

     //*******************************************
     // if width specified, skip remaining chars  
     //*******************************************

     if FORMATTED then
       while TRUE do
         begin
           if EOLN ( F ) or EOF ( F ) or ( WIDTH <= 0 ) then
             break ;
           GET ( F ) ;
           WIDTH := WIDTH - 1 ;
         end (* while *) ;
   end (* $PASRDR *) ;



function $PASRDI ( var F : TEXT ; WIDTH : INTEGER ) : INTEGER ;

//**********************************************************
// function to read integers from files                     
// rewritten in Pascal - portable solution in 2020          
// supports width parameter like in Pascal/VS               
//**********************************************************
// WIDTH parameter added to support fixed length integer    
// read - traditional variable length integer read specifies
// minus one, generated by the compiler                     
//**********************************************************
// Bernd Oppolzer - New Stanford Pascal                     
//**********************************************************


   var SIGN : INTEGER ;
       RETVAL : INTEGER ;
       FORMATTED : BOOLEAN ;

   begin (* $PASRDI *)
     if WIDTH = 0 then
       begin
         $PASRDI := 0 ;
         return
       end (* then *) ;
     FORMATTED := TRUE ;
     if WIDTH < 0 then
       begin
         FORMATTED := FALSE ;
         WIDTH := 999999999 ;
       end (* then *) ;
     if FILESTAT ( F ) = '0' then
       RESET ( F ) ;
     if EOLN ( F ) then
       GET ( F ) ;
     if EOF ( F ) then
       begin
         $PASRDI := 0 ;
         return
       end (* then *) ;
     SIGN := 1 ;
     RETVAL := 0 ;

     //**********************
     // skip leading blanks  
     //**********************

     while ( F -> = ' ' ) and ( WIDTH > 0 ) do
       begin
         if FORMATTED then
           begin
             GET ( F ) ;
             WIDTH := WIDTH - 1 ;
             if EOLN ( F ) or EOF ( F ) or ( WIDTH <= 0 ) then
               $ERROR ( 1203 ) ;
           end (* then *)
         else
           begin
             GET ( F ) ;
             if EOF ( F ) then
               begin
                 $PASRDI := RETVAL ;
                 return
               end (* then *)
           end (* else *)
       end (* while *) ;
     case F -> of
       '-' : begin
               SIGN := - 1 ;
               GET ( F ) ;
               WIDTH := WIDTH - 1 ;
               if WIDTH <= 0 then
                 $ERROR ( 1203 ) ;
               if not ( F -> in [ '0' .. '9' ] ) then
                 $ERROR ( 1203 ) ;
             end (* tag/ca *) ;
       '+' : begin
               GET ( F ) ;
               WIDTH := WIDTH - 1 ;
               if WIDTH <= 0 then
                 $ERROR ( 1203 ) ;
               if not ( F -> in [ '0' .. '9' ] ) then
                 $ERROR ( 1203 ) ;
             end (* tag/ca *) ;
       '0' .. '9' :
         begin
           RETVAL := ORD ( F -> ) - ORD ( '0' ) ;
           GET ( F ) ;
           WIDTH := WIDTH - 1 ;
         end (* tag/ca *) ;
       otherwise
         $ERROR ( 1203 )
     end (* case *) ;
     if EOLN ( F ) or ( WIDTH <= 0 ) then
       begin
         $PASRDI := RETVAL ;
         return
       end (* then *) ;

     //*************************************************
     // main loop                                       
     //*************************************************

     while ( F -> in [ '0' .. '9' ] ) and ( WIDTH > 0 ) do
       begin
         RETVAL := RETVAL * 10 + ( ORD ( F -> ) - ORD ( '0' ) ) ;
         GET ( F ) ;
         WIDTH := WIDTH - 1 ;
       end (* while *) ;

     //*******************************************
     // compose final result                      
     //*******************************************

     $PASRDI := RETVAL * SIGN ;

     //*******************************************
     // if width specified, skip remaining chars  
     //*******************************************

     if FORMATTED then
       while TRUE do
         begin
           if EOLN ( F ) or EOF ( F ) or ( WIDTH <= 0 ) then
             break ;
           GET ( F ) ;
           WIDTH := WIDTH - 1 ;
         end (* while *) ;
   end (* $PASRDI *) ;



function $PASRDB ( var F : TEXT ; WIDTH : INTEGER ) : BOOLEAN ;

//**********************************************************
// function to read booleans from files                     
// rewritten in Pascal - portable solution in 2020          
// supports width parameter like in Pascal/VS               
//**********************************************************
// WIDTH parameter added to support fixed length integer    
// read - traditional variable length integer read specifies
// minus one, generated by the compiler                     
//**********************************************************
// Bernd Oppolzer - New Stanford Pascal                     
//**********************************************************


   var RETVAL : BOOLEAN ;
       FORMATTED : BOOLEAN ;
       IX : INTEGER ;
       BUFFER : CHAR ( 5 ) ;
       WIDTHMAX : INTEGER ;

   begin (* $PASRDB *)
     if WIDTH = 0 then
       begin
         $PASRDB := FALSE ;
         return
       end (* then *) ;
     FORMATTED := TRUE ;
     if WIDTH < 0 then
       begin
         FORMATTED := FALSE ;
         WIDTH := 999999999 ;
       end (* then *) ;
     if FILESTAT ( F ) = '0' then
       RESET ( F ) ;
     if EOLN ( F ) then
       GET ( F ) ;
     if EOF ( F ) then
       begin
         $PASRDB := FALSE ;
         return
       end (* then *) ;
     RETVAL := FALSE ;

     //**********************
     // skip leading blanks  
     //**********************

     while ( F -> = ' ' ) and ( WIDTH > 0 ) do
       begin
         if FORMATTED then
           begin
             GET ( F ) ;
             WIDTH := WIDTH - 1 ;
             if EOLN ( F ) or EOF ( F ) or ( WIDTH <= 0 ) then
               $ERROR ( 1209 )
           end (* then *)
         else
           begin
             GET ( F ) ;
             if EOF ( F ) then
               begin
                 $PASRDB := RETVAL ;
                 return
               end (* then *)
           end (* else *)
       end (* while *) ;

     //************************************************
     // now check for T or F                           
     // if formatted input (length specified)          
     // and length > 0, the word in the area must be   
     // any abbreviation of the words TRUE or FALSE    
     // as it may have been produced by write (boolean)
     // no other characters than blanks may occur      
     // before and after this word. the input is       
     // format free.                                   
     //************************************************

     case F -> of
       'F' , 'f' :
         begin
           if FORMATTED and ( WIDTH > 1 ) then
             begin
               BUFFER := 'FALSE' ;
               WIDTHMAX := WIDTH ;
               if WIDTHMAX > 5 then
                 WIDTHMAX := 5 ;
               IX := 1 ;
               repeat
                 BUFFER [ IX ] := TOUPPER ( F -> ) ;
                 IX := IX + 1 ;
                 if EOLN ( F ) or EOF ( F ) then
                   break ;
                 GET ( F ) ;
                 WIDTH := WIDTH - 1
               until ( IX > WIDTHMAX ) or ( not ( F -> in BUCHST ) ) ;
               if BUFFER <> 'FALSE' then
                 $ERROR ( 1210 ) ;
               RETVAL := FALSE ;
             end (* then *)
           else
             begin
               RETVAL := FALSE ;
               GET ( F ) ;
               if FORMATTED then
                 WIDTH := WIDTH - 1 ;
             end (* else *) ;
         end (* tag/ca *) ;
       'T' , 't' :
         begin
           if FORMATTED and ( WIDTH > 1 ) then
             begin
               BUFFER := 'TRUE' ;
               WIDTHMAX := WIDTH ;
               if WIDTHMAX > 4 then
                 WIDTHMAX := 4 ;
               IX := 1 ;
               repeat
                 BUFFER [ IX ] := TOUPPER ( F -> ) ;
                 IX := IX + 1 ;
                 if EOLN ( F ) or EOF ( F ) then
                   break ;
                 GET ( F ) ;
                 WIDTH := WIDTH - 1
               until ( IX > WIDTHMAX ) or ( not ( F -> in BUCHST ) ) ;
               if BUFFER <> 'TRUE' then
                 $ERROR ( 1210 ) ;
               RETVAL := TRUE ;
             end (* then *)
           else
             begin
               RETVAL := TRUE ;
               GET ( F ) ;
               if FORMATTED then
                 WIDTH := WIDTH - 1 ;
             end (* else *) ;
         end (* tag/ca *) ;
       otherwise
         $ERROR ( 1209 )
     end (* case *) ;

     //*******************************************
     // set result                                
     //*******************************************

     $PASRDB := RETVAL ;
     if EOLN ( F ) or ( WIDTH <= 0 ) then
       return ;

     //*******************************************
     // if width specified, skip remaining chars  
     //*******************************************

     if FORMATTED then
       while TRUE do
         begin
           if EOLN ( F ) or EOF ( F ) or ( WIDTH <= 0 ) then
             break ;
           if F -> <> ' ' then
             $ERROR ( 1211 ) ;
           GET ( F ) ;
           WIDTH := WIDTH - 1 ;
         end (* while *) ;
   end (* $PASRDB *) ;



function $PASRDX ( var F : TEXT ; PMETA : ANYPTR ; WIDTH : INTEGER ) :
                 INTEGER ;

//**********************************************************
// function to scalar values from files                     
// this does not exist in Standard Pascal                   
//**********************************************************
// WIDTH parameter added to support fixed length integer    
// minus one means: no length specified                     
//**********************************************************
// a runtime error is generated if the length is too small  
// with respect to the minimum which is required for the    
// scalar type (minimum unique length).                     
//**********************************************************
// Bernd Oppolzer - New Stanford Pascal                     
//**********************************************************


   type SHORT = 0 .. 32000 ;
        PSHORT = -> SHORT ;

   var PS : PSHORT ;
       RETVAL : INTEGER ;
       FORMATTED : BOOLEAN ;
       BUFFER : CHAR ( 20 ) ;
       IX : INTEGER ;
       WIDTHMAX : INTEGER ;
       ELEMCOUNT : SHORT ;
       MAXIDLEN : SHORT ;
       MINIDLEN : SHORT ;

   begin (* $PASRDX *)
     if WIDTH = 0 then
       begin
         $PASRDX := 0 ;
         return
       end (* then *) ;

     //**********************************
     // access metadata for scalar type  
     //**********************************

     PS := PMETA ;
     if PS -> <> 0 then
       $ERROR ( 1220 ) ;
     PS := PTRADD ( PS , 2 ) ;
     ELEMCOUNT := PS -> ;
     PS := PTRADD ( PS , 2 ) ;
     MAXIDLEN := PS -> ;
     PS := PTRADD ( PS , 2 ) ;
     MINIDLEN := PS -> ;
     PS := PTRADD ( PS , 2 ) ;

     //**********************************
     // check width                      
     //**********************************

     FORMATTED := TRUE ;
     if WIDTH < 0 then
       begin
         FORMATTED := FALSE ;
         WIDTH := 999999999 ;
       end (* then *) ;
     if FILESTAT ( F ) = '0' then
       RESET ( F ) ;
     if EOLN ( F ) then
       GET ( F ) ;
     if EOF ( F ) then
       begin
         $PASRDX := 0 ;
         return
       end (* then *) ;
     RETVAL := 0 ;

     //**********************
     // skip leading blanks  
     //**********************

     while ( F -> = ' ' ) and ( WIDTH > 0 ) do
       begin
         if FORMATTED then
           begin
             GET ( F ) ;
             WIDTH := WIDTH - 1 ;
             if EOLN ( F ) or EOF ( F ) or ( WIDTH <= 0 ) then
               $ERROR ( 1212 )
           end (* then *)
         else
           begin
             GET ( F ) ;
             if EOF ( F ) then
               begin
                 $PASRDX := RETVAL ;
                 return
               end (* then *)
           end (* else *)
       end (* while *) ;

     //************************************************
     // now read the id of the scalar value            
     // at most MAXIDLEN chars are read                
     //************************************************

     if not ( F -> in IDCHARS ) then
       $ERROR ( 1212 ) ;
     BUFFER := ' ' ;
     WIDTHMAX := WIDTH ;
     if WIDTHMAX > 20 then
       WIDTHMAX := 20 ;
     if WIDTHMAX > MAXIDLEN then
       WIDTHMAX := MAXIDLEN ;
     IX := 1 ;
     repeat
       BUFFER [ IX ] := TOUPPER ( F -> ) ;
       IX := IX + 1 ;
       if EOLN ( F ) or EOF ( F ) then
         break ;
       GET ( F ) ;
       WIDTH := WIDTH - 1
     until ( IX > WIDTHMAX ) or ( not ( F -> in IDCHARS ) ) ;
     IX := IX - 1 ;
     if IX < MINIDLEN then
       $ERROR ( 1214 ) ;

     //*******************************************
     // now the string in BUFFER is the scalar    
     // identifier (maybe) and has a length       
     // between MINIDLEN and MAXIDLEN;            
     // do a binary search in the PMETA table     
     // to find the value of the scalar           
     //*******************************************


     //*******************************************
     // set result                                
     //*******************************************

     $PASRDX := RETVAL ;
     if EOLN ( F ) or ( WIDTH <= 0 ) then
       return ;

     //*******************************************
     // if width specified, skip remaining chars  
     //*******************************************

     if FORMATTED then
       while TRUE do
         begin
           if EOLN ( F ) or EOF ( F ) or ( WIDTH <= 0 ) then
             break ;
           if F -> <> ' ' then
             $ERROR ( 1213 ) ;
           GET ( F ) ;
           WIDTH := WIDTH - 1 ;
         end (* while *) ;
   end (* $PASRDX *) ;



function $PASRSC ( const S : STRING ;         // source string
                 var RPOS : INTEGER ;         // reading position
                 WIDTH : INTEGER ) : CHAR ;   // optional width

//************************************************************
// this function implements READSTR for single chars          
// s = the source string                                      
// rpos = the reading position (counts from 1) - is updated   
// width = the optional width (default -1)                    
//************************************************************


   var RES : CHAR ;

   begin (* $PASRSC *)
     if WIDTH = 0 then
       begin
         $PASRSC := ' ' ;
         return
       end (* then *) ;
     if RPOS <= 0 then
       RPOS := 1 ;
     if RPOS > LENGTH ( S ) then
       begin
         $PASRSC := ' ' ;
         return
       end (* then *) ;
     RES := S [ RPOS ] ;
     if WIDTH < 0 then
       RPOS := RPOS + 1
     else
       RPOS := RPOS + WIDTH ;
     $PASRSC := RES
   end (* $PASRSC *) ;



procedure $PASRSS ( const S : STRING ;     // source string
                  var RPOS : INTEGER ;     // reading position
                  WIDTH : INTEGER ;        // optional width
                  RES : VOIDPTR ;          // pointer to target
                  LEN : INTEGER ) ;        // len of target

//************************************************************
// this function implements READSTR for character arrays      
// s = the source string                                      
// rpos = the reading position (counts from 1) - is updated   
// width = the optional width (default -1)                    
// res = address of target char array                         
// len = length of target char array                          
//************************************************************


   var CP_TARGET : -> CHAR ;
       CP_SOURCE : -> CHAR ;
       RLEN : INTEGER ;

   begin (* $PASRSS *)
     CP_TARGET := RES ;
     MEMSET ( CP_TARGET , ' ' , LEN ) ;
     if WIDTH = 0 then
       return ;
     if RPOS <= 0 then
       RPOS := 1 ;
     if RPOS > LENGTH ( S ) then
       return ;
     CP_SOURCE := ADDR ( S [ RPOS ] ) ;
     RLEN := LENGTH ( S ) - RPOS + 1 ;
     if RLEN < LEN then
       LEN := RLEN ;
     if WIDTH > 0 then
       if WIDTH < LEN then
         LEN := WIDTH ;
     MEMCPY ( CP_TARGET , CP_SOURCE , LEN ) ;
     if WIDTH < 0 then
       RPOS := RPOS + LEN
     else
       RPOS := RPOS + WIDTH ;
   end (* $PASRSS *) ;



procedure $PASRSV ( const S : STRING ;     // source string
                  var RPOS : INTEGER ;     // reading position
                  WIDTH : INTEGER ;        // optional width
                  var RES : STRING ) ;     // target string

//************************************************************
// this function implements READSTR for strings (varchars)    
// s = the source string                                      
// rpos = the reading position (counts from 1) - is updated   
// width = the optional width (default -1)                    
// res = target string (var parameter, conformant string)     
// maximum length of target string = maxlength of res         
//************************************************************


   var LEN : INTEGER ;
       RLEN : INTEGER ;

   begin (* $PASRSV *)
     LEN := MAXLENGTH ( RES ) ;
     RES := '' ;
     if WIDTH = 0 then
       return ;
     if RPOS <= 0 then
       RPOS := 1 ;
     if RPOS > LENGTH ( S ) then
       return ;
     RLEN := LENGTH ( S ) - RPOS + 1 ;
     if RLEN < LEN then
       LEN := RLEN ;
     if WIDTH > 0 then
       if WIDTH < LEN then
         LEN := WIDTH ;
     RES := SUBSTR ( S , RPOS , LEN ) ;
     if WIDTH < 0 then
       RPOS := RPOS + LEN
     else
       RPOS := RPOS + WIDTH ;
   end (* $PASRSV *) ;



function $PASRSB ( const S : STRING ;     // source string
                 var RPOS : INTEGER ;     // reading position
                 WIDTH : INTEGER )        // optional width
                 : BOOLEAN ;              // result type

//**********************************************************
// this function implements READSTR for booleans            
// s = the source string                                    
// rpos = the reading position (counts from 1) - is updated 
// width = the optional width (default -1)                  
//**********************************************************
// Bernd Oppolzer - New Stanford Pascal                     
//**********************************************************


   var RETVAL : BOOLEAN ;
       FORMATTED : BOOLEAN ;
       F : -> CHAR ;
       RLEN : INTEGER ;
       IX : INTEGER ;
       BUFFER : CHAR ( 5 ) ;
       WIDTHMAX : INTEGER ;

   begin (* $PASRSB *)
     if WIDTH = 0 then
       begin
         $PASRSB := FALSE ;
         return
       end (* then *) ;
     FORMATTED := TRUE ;
     if WIDTH < 0 then
       begin
         FORMATTED := FALSE ;
         WIDTH := 999999999 ;
       end (* then *) ;
     if RPOS <= 0 then
       RPOS := 1 ;
     if RPOS > LENGTH ( S ) then
       begin
         $PASRSB := FALSE ;
         return
       end (* then *) ;
     F := ADDR ( S [ RPOS ] ) ;
     RLEN := LENGTH ( S ) - RPOS + 1 ;
     RETVAL := FALSE ;

     //**********************
     // skip leading blanks  
     //**********************

     while ( F -> = ' ' ) and ( WIDTH > 0 ) and ( RLEN > 0 ) do
       begin
         F := PTRADD ( F , 1 ) ;
         RLEN := RLEN - 1 ;
         WIDTH := WIDTH - 1 ;
       end (* while *) ;
     if ( RLEN <= 0 ) or ( WIDTH <= 0 ) then
       begin
         RPOS := PTRDIFF ( F , ADDR ( S [ 1 ] ) ) + 1 ;
         $PASRSB := RETVAL ;
         return
       end (* then *) ;

     //************************************************
     // now check for T or F                           
     // if formatted input (length specified)          
     // and length > 0, the word in the area must be   
     // any abbreviation of the words TRUE or FALSE    
     // as it may have been produced by write (boolean)
     // no other characters than blanks may occur      
     // before and after this word. the input is       
     // format free.                                   
     //************************************************

     case F -> of
       'F' , 'f' :
         begin
           if FORMATTED and ( WIDTH > 1 ) then
             begin
               BUFFER := 'FALSE' ;
               WIDTHMAX := WIDTH ;
               if WIDTHMAX > 5 then
                 WIDTHMAX := 5 ;
               IX := 1 ;
               repeat
                 BUFFER [ IX ] := TOUPPER ( F -> ) ;
                 IX := IX + 1 ;
                 if ( WIDTH > 0 ) and ( RLEN > 0 ) then
                   begin
                     F := PTRADD ( F , 1 ) ;
                     RLEN := RLEN - 1 ;
                     WIDTH := WIDTH - 1 ;
                   end (* then *) ;
               until ( IX > WIDTHMAX ) or ( F -> = ' ' ) or ( WIDTH <=
               0 ) or ( RLEN <= 0 ) ;
               if BUFFER <> 'FALSE' then
                 $ERROR ( 1210 ) ;
               RETVAL := FALSE ;
             end (* then *)
           else
             begin
               RETVAL := FALSE ;
               F := PTRADD ( F , 1 ) ;
               if FORMATTED then
                 WIDTH := WIDTH - 1 ;
             end (* else *) ;
         end (* tag/ca *) ;
       'T' , 't' :
         begin
           if FORMATTED and ( WIDTH > 1 ) then
             begin
               BUFFER := 'TRUE' ;
               WIDTHMAX := WIDTH ;
               if WIDTHMAX > 4 then
                 WIDTHMAX := 4 ;
               IX := 1 ;
               repeat
                 BUFFER [ IX ] := TOUPPER ( F -> ) ;
                 IX := IX + 1 ;
                 if ( WIDTH > 0 ) and ( RLEN > 0 ) then
                   begin
                     F := PTRADD ( F , 1 ) ;
                     RLEN := RLEN - 1 ;
                     WIDTH := WIDTH - 1 ;
                   end (* then *) ;
               until ( IX > WIDTHMAX ) or ( F -> = ' ' ) or ( WIDTH <=
               0 ) or ( RLEN <= 0 ) ;
               if BUFFER <> 'TRUE' then
                 $ERROR ( 1210 ) ;
               RETVAL := TRUE ;
             end (* then *)
           else
             begin
               RETVAL := TRUE ;
               F := PTRADD ( F , 1 ) ;
               if FORMATTED then
                 WIDTH := WIDTH - 1 ;
             end (* else *) ;
         end (* tag/ca *) ;
       otherwise
         $ERROR ( 1209 )
     end (* case *) ;

     //*******************************************
     // compose final result                      
     //*******************************************

     $PASRSB := RETVAL ;

     //*******************************************
     // if width specified, skip remaining chars  
     //*******************************************

     if FORMATTED then
       while TRUE do
         begin
           if ( RLEN <= 0 ) or ( WIDTH <= 0 ) then
             break ;
           if F -> <> ' ' then
             $ERROR ( 1211 ) ;
           F := PTRADD ( F , 1 ) ;
           RLEN := RLEN - 1 ;
           WIDTH := WIDTH - 1 ;
         end (* while *) ;
     RPOS := PTRDIFF ( F , ADDR ( S [ 1 ] ) ) + 1 ;
   end (* $PASRSB *) ;



function $PASRSI ( const S : STRING ;     // source string
                 var RPOS : INTEGER ;     // reading position
                 WIDTH : INTEGER )        // optional width
                 : INTEGER ;              // result type

//**********************************************************
// this function implements READSTR for integers            
// s = the source string                                    
// rpos = the reading position (counts from 1) - is updated 
// width = the optional width (default -1)                  
//**********************************************************
// Bernd Oppolzer - New Stanford Pascal                     
//**********************************************************


   var SIGN : INTEGER ;
       RETVAL : INTEGER ;
       FORMATTED : BOOLEAN ;
       F : -> CHAR ;
       RLEN : INTEGER ;

   begin (* $PASRSI *)
     if WIDTH = 0 then
       begin
         $PASRSI := 0 ;
         return
       end (* then *) ;
     FORMATTED := TRUE ;
     if WIDTH < 0 then
       begin
         FORMATTED := FALSE ;
         WIDTH := 999999999 ;
       end (* then *) ;
     if RPOS <= 0 then
       RPOS := 1 ;
     if RPOS > LENGTH ( S ) then
       begin
         $PASRSI := 0 ;
         return
       end (* then *) ;
     F := ADDR ( S [ RPOS ] ) ;
     RLEN := LENGTH ( S ) - RPOS + 1 ;
     SIGN := 1 ;
     RETVAL := 0 ;

     //**********************
     // skip leading blanks  
     //**********************

     while ( F -> = ' ' ) and ( WIDTH > 0 ) and ( RLEN > 0 ) do
       begin
         F := PTRADD ( F , 1 ) ;
         RLEN := RLEN - 1 ;
         WIDTH := WIDTH - 1 ;
       end (* while *) ;
     if ( RLEN <= 0 ) or ( WIDTH <= 0 ) then
       begin
         RPOS := PTRDIFF ( F , ADDR ( S [ 1 ] ) ) + 1 ;
         $PASRSI := RETVAL ;
         return
       end (* then *) ;
     case F -> of
       '-' : begin
               SIGN := - 1 ;
               F := PTRADD ( F , 1 ) ;
               RLEN := RLEN - 1 ;
               WIDTH := WIDTH - 1 ;
               if WIDTH <= 0 then
                 $ERROR ( 1203 ) ;
               if RLEN <= 0 then
                 $ERROR ( 1203 ) ;
               if not ( F -> in [ '0' .. '9' ] ) then
                 $ERROR ( 1203 ) ;
             end (* tag/ca *) ;
       '+' : begin
               F := PTRADD ( F , 1 ) ;
               RLEN := RLEN - 1 ;
               WIDTH := WIDTH - 1 ;
               if WIDTH <= 0 then
                 $ERROR ( 1203 ) ;
               if RLEN <= 0 then
                 $ERROR ( 1203 ) ;
               if not ( F -> in [ '0' .. '9' ] ) then
                 $ERROR ( 1203 ) ;
             end (* tag/ca *) ;
       '0' .. '9' :
         begin
           RETVAL := ORD ( F -> ) - ORD ( '0' ) ;
           F := PTRADD ( F , 1 ) ;
           RLEN := RLEN - 1 ;
           WIDTH := WIDTH - 1 ;
         end (* tag/ca *) ;
       otherwise
         $ERROR ( 1203 )
     end (* case *) ;
     if ( RLEN <= 0 ) or ( WIDTH <= 0 ) then
       begin
         RPOS := PTRDIFF ( F , ADDR ( S [ 1 ] ) ) + 1 ;
         $PASRSI := RETVAL ;
         return
       end (* then *) ;

     //*************************************************
     // main loop                                       
     //*************************************************

     while ( F -> in [ '0' .. '9' ] ) and ( WIDTH > 0 ) and ( RLEN > 0
     ) do
       begin
         RETVAL := RETVAL * 10 + ( ORD ( F -> ) - ORD ( '0' ) ) ;
         F := PTRADD ( F , 1 ) ;
         RLEN := RLEN - 1 ;
         WIDTH := WIDTH - 1 ;
       end (* while *) ;

     //*******************************************
     // compose final result                      
     //*******************************************

     $PASRSI := RETVAL * SIGN ;

     //*******************************************
     // if width specified, skip remaining chars  
     //*******************************************

     if FORMATTED then
       while TRUE do
         begin
           if ( RLEN <= 0 ) or ( WIDTH <= 0 ) then
             break ;
           F := PTRADD ( F , 1 ) ;
           RLEN := RLEN - 1 ;
           WIDTH := WIDTH - 1 ;
         end (* while *) ;
     RPOS := PTRDIFF ( F , ADDR ( S [ 1 ] ) ) + 1 ;
   end (* $PASRSI *) ;



function $PASRSR ( const S : STRING ;     // source string
                 var RPOS : INTEGER ;     // reading position
                 WIDTH : INTEGER )        // optional width
                 : REAL ;                 // result type

//**********************************************************
// this function implements READSTR for reals               
// s = the source string                                    
// rpos = the reading position (counts from 1) - is updated 
// width = the optional width (default -1)                  
//**********************************************************
// Bernd Oppolzer - New Stanford Pascal                     
//**********************************************************


   var SIGN : INTEGER ;
       RETVAL : REAL ;
       RETVALI : INTEGER ;
       SCALE : REAL ;
       LEADINGZ : BOOLEAN ;
       DIGITS : INTEGER ;
       BEFOREDP : BOOLEAN ;
       FORMATTED : BOOLEAN ;
       F : -> CHAR ;
       RLEN : INTEGER ;
       EXPO : BOOLEAN ;
       SCALEXP : REAL ;
       EXPVALI : INTEGER ;
       SIGNEXP : INTEGER ;

   const SCALTAB_PLUS : array [ 1 .. 10 ] of REAL =
         ( 10.0 , 100.0 , 1000.0 , 10000.0 , 100000.0 , 1000000.0 ,
           10000000.0 , 100000000.0 , 1000000000.0 , 10000000000.0 ) ;
         SCALTAB_MINUS : array [ 1 .. 10 ] of REAL =
         ( 0.1 , 0.01 , 0.001 , 0.0001 , 0.00001 , 0.000001 , 0.0000001
           , 0.00000001 , 0.000000001 , 0.0000000001 ) ;

   begin (* $PASRSR *)
     if WIDTH = 0 then
       begin
         $PASRSR := 0.0 ;
         return
       end (* then *) ;
     FORMATTED := TRUE ;
     if WIDTH < 0 then
       begin
         FORMATTED := FALSE ;
         WIDTH := 999999999 ;
       end (* then *) ;
     if RPOS <= 0 then
       RPOS := 1 ;
     if RPOS > LENGTH ( S ) then
       begin
         $PASRSR := 0.0 ;
         return
       end (* then *) ;
     F := ADDR ( S [ RPOS ] ) ;
     RLEN := LENGTH ( S ) - RPOS + 1 ;
     SIGN := 1 ;
     RETVAL := 0.0 ;
     RETVALI := 0 ;

     //**********************
     // skip leading blanks  
     //**********************

     while ( F -> = ' ' ) and ( WIDTH > 0 ) and ( RLEN > 0 ) do
       begin
         F := PTRADD ( F , 1 ) ;
         RLEN := RLEN - 1 ;
         WIDTH := WIDTH - 1 ;
       end (* while *) ;
     if ( RLEN <= 0 ) or ( WIDTH <= 0 ) then
       begin
         RPOS := PTRDIFF ( F , ADDR ( S [ 1 ] ) ) + 1 ;
         $PASRSR := RETVAL ;
         return
       end (* then *) ;

     //********************************************************
     // leadingz = reading and ignoring leading zeroes         
     // digits = number of digits (if < 10 then integer math)  
     // beforedp = decimal point not yet found                 
     // scale = compute scale factor from dec point            
     //********************************************************

     LEADINGZ := TRUE ;
     DIGITS := 0 ;
     BEFOREDP := TRUE ;
     SCALE := 1.0 ;
     EXPO := FALSE ;
     case F -> of
       '-' : begin
               SIGN := - 1 ;
               F := PTRADD ( F , 1 ) ;
               RLEN := RLEN - 1 ;
               WIDTH := WIDTH - 1 ;
               if WIDTH <= 0 then
                 $ERROR ( 1204 ) ;
               if RLEN <= 0 then
                 $ERROR ( 1204 ) ;
               if not ( F -> in [ '0' .. '9' ] ) then
                 $ERROR ( 1204 ) ;
             end (* tag/ca *) ;
       '+' : begin
               F := PTRADD ( F , 1 ) ;
               RLEN := RLEN - 1 ;
               WIDTH := WIDTH - 1 ;
               if WIDTH <= 0 then
                 $ERROR ( 1204 ) ;
               if RLEN <= 0 then
                 $ERROR ( 1204 ) ;
               if not ( F -> in [ '0' .. '9' ] ) then
                 $ERROR ( 1204 ) ;
             end (* tag/ca *) ;
       '.' : begin
               F := PTRADD ( F , 1 ) ;
               RLEN := RLEN - 1 ;
               WIDTH := WIDTH - 1 ;
               BEFOREDP := FALSE ;
             end (* tag/ca *) ;
       '0' .. '9' :
         begin
           RETVALI := ORD ( F -> ) - ORD ( '0' ) ;
           LEADINGZ := RETVALI = 0 ;
           if not LEADINGZ then
             DIGITS := 1 ;
           F := PTRADD ( F , 1 ) ;
           RLEN := RLEN - 1 ;
           WIDTH := WIDTH - 1 ;
         end (* tag/ca *) ;
       otherwise
         $ERROR ( 1204 )
     end (* case *) ;
     if ( RLEN <= 0 ) or ( WIDTH <= 0 ) then
       begin
         RPOS := PTRDIFF ( F , ADDR ( S [ 1 ] ) ) + 1 ;
         RETVAL := RETVALI ;
         $PASRSR := RETVAL ;
         return
       end (* then *) ;

     //*************************************************
     // main loop for mantissa                          
     // logic controlled by leadingz, digits, beforedp  
     // final results in retval and scale               
     //*************************************************

     while ( WIDTH > 0 ) and ( RLEN > 0 ) do
       begin
         case F -> of
           '.' : begin
                   if not BEFOREDP then
                     $ERROR ( 1205 ) ;
                   F := PTRADD ( F , 1 ) ;
                   RLEN := RLEN - 1 ;
                   WIDTH := WIDTH - 1 ;
                   BEFOREDP := FALSE ;
                 end (* tag/ca *) ;
           'e' , 'E' :
             begin
               F := PTRADD ( F , 1 ) ;
               RLEN := RLEN - 1 ;
               WIDTH := WIDTH - 1 ;
               EXPO := TRUE ;
               break ;
             end (* tag/ca *) ;
           '0' .. '9' :
             begin
               if not BEFOREDP then
                 SCALE := SCALE * 10.0 ;
               if F -> <> '0' then
                 LEADINGZ := FALSE ;
               if not LEADINGZ then
                 begin
                   DIGITS := DIGITS + 1 ;
                   if DIGITS < 10 then
                     begin
                       RETVALI := RETVALI * 10 + ( ORD ( F -> ) - ORD (
                                  '0' ) ) ;
                       if DIGITS = 9 then
                         RETVAL := RETVALI ;
                     end (* then *)
                   else
                     begin
                       RETVAL := RETVAL * 10 + ( ORD ( F -> ) - ORD (
                                 '0' ) ) ;
                     end (* else *) ;
                 end (* then *) ;
               F := PTRADD ( F , 1 ) ;
               RLEN := RLEN - 1 ;
               WIDTH := WIDTH - 1 ;
             end (* tag/ca *) ;
           otherwise
             break
         end (* case *) ;
       end (* while *) ;

     //*******************************************************
     // if real value fields have not been set, set them now  
     //*******************************************************

     if DIGITS < 9 then
       RETVAL := RETVALI ;

     //*******************************
     // read exponent if expo = true  
     // and compose final result      
     //*******************************

     SCALEXP := 1.0 ;
     if EXPO then
       begin

     //*******************************
     // E has already been found      
     // exponent may have sign        
     //*******************************

         SIGNEXP := 1 ;
         EXPVALI := 0 ;
         case F -> of
           '-' : begin
                   SIGNEXP := - 1 ;
                   F := PTRADD ( F , 1 ) ;
                   RLEN := RLEN - 1 ;
                   WIDTH := WIDTH - 1 ;
                   if WIDTH <= 0 then
                     $ERROR ( 1206 ) ;
                   if RLEN <= 0 then
                     $ERROR ( 1206 ) ;
                   if not ( F -> in [ '0' .. '9' ] ) then
                     $ERROR ( 1206 ) ;
                 end (* tag/ca *) ;
           '+' : begin
                   F := PTRADD ( F , 1 ) ;
                   RLEN := RLEN - 1 ;
                   WIDTH := WIDTH - 1 ;
                   if WIDTH <= 0 then
                     $ERROR ( 1206 ) ;
                   if RLEN <= 0 then
                     $ERROR ( 1206 ) ;
                   if not ( F -> in [ '0' .. '9' ] ) then
                     $ERROR ( 1206 ) ;
                 end (* tag/ca *) ;
           '0' .. '9' :
             begin
               EXPVALI := ORD ( F -> ) - ORD ( '0' ) ;
               F := PTRADD ( F , 1 ) ;
               RLEN := RLEN - 1 ;
               WIDTH := WIDTH - 1 ;
             end (* tag/ca *) ;
           otherwise
             $ERROR ( 1206 )
         end (* case *) ;
         while ( F -> in [ '0' .. '9' ] ) and ( RLEN > 0 ) and ( WIDTH
         > 0 ) do
           begin
             EXPVALI := EXPVALI * 10 + ( ORD ( F -> ) - ORD ( '0' ) ) ;
             F := PTRADD ( F , 1 ) ;
             RLEN := RLEN - 1 ;
             WIDTH := WIDTH - 1 ;
           end (* while *) ;

     //*******************************************
     // compute scalexp depending on exponent     
     //*******************************************

         if SIGNEXP = - 1 then
           begin
             while EXPVALI > 10 do
               begin
                 SCALEXP := SCALEXP * SCALTAB_MINUS [ 10 ] ;
                 EXPVALI := EXPVALI - 10
               end (* while *) ;
             SCALEXP := SCALEXP * SCALTAB_MINUS [ EXPVALI ] ;
           end (* then *)
         else
           begin
             while EXPVALI > 10 do
               begin
                 SCALEXP := SCALEXP * SCALTAB_PLUS [ 10 ] ;
                 EXPVALI := EXPVALI - 10
               end (* while *) ;
             SCALEXP := SCALEXP * SCALTAB_PLUS [ EXPVALI ] ;
           end (* else *) ;

     //*******************************************
     // compose final result with scalexp         
     //*******************************************

         $PASRSR := RETVAL / SCALE * SCALEXP * SIGN ;
       end (* then *)
     else
       $PASRSR := RETVAL / SCALE * SIGN ;

     //*******************************************
     // if width specified, skip remaining chars  
     //*******************************************

     if FORMATTED then
       while TRUE do
         begin
           if ( RLEN <= 0 ) or ( WIDTH <= 0 ) then
             break ;
           F := PTRADD ( F , 1 ) ;
           RLEN := RLEN - 1 ;
           WIDTH := WIDTH - 1 ;
         end (* while *) ;
     RPOS := PTRDIFF ( F , ADDR ( S [ 1 ] ) ) + 1 ;
   end (* $PASRSR *) ;



procedure $PASWRI ( var F : TEXT ; WIDTH : INTEGER ; V : INTEGER ) ;

//**********************************************************
// write integers to files                                  
// rewritten in Pascal - portable solution in 2020          
//**********************************************************
// Bernd Oppolzer - New Stanford Pascal                     
//**********************************************************


   var BUFFER : CHAR ( 12 ) ;
       BUFFOUT : CHAR ( 12 ) ;
       CP : -> CHAR ;
       MINUS : BOOLEAN ;
       LEADZ : BOOLEAN ;
       L : INTEGER ;
       ZEROES : INTEGER ;
       X : INTEGER ;
       DIG : CHAR ;

   begin (* $PASWRI *)
     if WIDTH = 0 then
       return ;
     LEADZ := FALSE ;
     if WIDTH < 0 then
       begin
         LEADZ := TRUE ;
         WIDTH := - WIDTH
       end (* then *) ;
     MINUS := FALSE ;
     BUFFER := ' ' ;
     CP := ADDR ( BUFFER [ 12 ] ) ;
     if V = 0 then
       begin
         BUFFER [ 12 ] := '0' ;
         CP := PTRADD ( CP , - 1 ) ;
       end (* then *)
     else
       if V < 0 then
         begin
           MINUS := TRUE ;
           V := - V
         end (* then *) ;
     while V > 0 do
       begin
         DIG := CHR ( ORD ( '0' ) + V MOD 10 ) ;
         CP -> := DIG ;
         CP := PTRADD ( CP , - 1 ) ;
         V := V DIV 10 ;
       end (* while *) ;
     L := PTRDIFF ( ADDR ( BUFFER [ 12 ] ) , CP ) ;
     CP := PTRADD ( CP , 1 ) ;
     MEMCPY ( ADDR ( BUFFOUT ) , CP , L ) ;
     if MINUS then
       L := L + 1 ;
     if L >= WIDTH then
       begin
         if MINUS then
           WRITE ( F , '-' ) ;
         WRITE ( F , BUFFOUT : L ) ;
       end (* then *)
     else
       begin
         ZEROES := WIDTH - L ;
         if LEADZ then
           begin
             if MINUS then
               WRITE ( F , '-' ) ;
             for X := 1 to ZEROES do
               WRITE ( F , '0' ) ;
             if MINUS then
               WRITE ( F , BUFFOUT : L - 1 )
             else
               WRITE ( F , BUFFOUT : L ) ;
           end (* then *)
         else
           begin
             WRITE ( F , ' ' : ZEROES ) ;
             if MINUS then
               begin
                 WRITE ( F , '-' ) ;
                 WRITE ( F , BUFFOUT : L - 1 ) ;
               end (* then *)
             else
               WRITE ( F , BUFFOUT : L ) ;
           end (* else *)
       end (* else *)
   end (* $PASWRI *) ;



procedure $PASWRR ( var F : TEXT ; WIDTH : INTEGER ; SCALE : INTEGER ;
                  V : REAL ) ;

//**********************************************************
// write reals to files                                     
// rewritten in Pascal - portable solution in 2020          
//**********************************************************
// Bernd Oppolzer - New Stanford Pascal                     
//**********************************************************


   var BUFFER : CHAR ( 100 ) ;
       MINUS : BOOLEAN ;
       LEADZ : BOOLEAN ;
       LEN : INTEGER ;
       DIG : INTEGER ;
       EXPO : CHAR ( 4 ) ;
       ZEROES : INTEGER ;
       X : INTEGER ;
       EXPONENT : INTEGER ;
       FERTIG : BOOLEAN ;

   begin (* $PASWRR *)
     if WIDTH = 0 then
       return ;
     if SCALE > 16 then
       $ERROR ( 1207 ) ;
     LEADZ := FALSE ;
     if WIDTH < 0 then
       begin
         LEADZ := TRUE ;
         WIDTH := - WIDTH
       end (* then *) ;
     MINUS := FALSE ;
     BUFFER := ' ' ;

     //*************************
     // output real value zero  
     //*************************

     if V = 0.0 then
       if SCALE < 0 then

     //*************************
     // write zero in E-Format  
     //*************************

         begin
           if WIDTH < 9 then
             begin
               BUFFER := '0.0E+00' ;
               LEN := 7 ;
               WIDTH := LEN + 1 ;
               LEADZ := FALSE ;
             end (* then *)
           else
             begin
               DIG := WIDTH - 7 ;
               if DIG > 16 then
                 DIG := 16 ;
               BUFFER := '0.' ;
               MEMSET ( ADDR ( BUFFER [ 3 ] ) , '0' , DIG ) ;
               EXPO := 'E+00' ;
               MEMCPY ( ADDR ( BUFFER [ 3 + DIG ] ) , ADDR ( EXPO ) , 4
                        ) ;
               LEN := DIG + 6 ;
               LEADZ := FALSE ;
             end (* else *)
         end (* then *)
       else

     //*********************************
     // write zero in F-Format          
     // no decimal point, if scale = 0  
     //*********************************

         if SCALE = 0 then
           begin
             BUFFER [ 1 ] := '0' ;
             LEN := 1 ;
           end (* then *)
         else
           begin
             MEMSET ( ADDR ( BUFFER ) , '0' , SCALE + 2 ) ;
             BUFFER [ 2 ] := '.' ;
             LEN := SCALE + 2 ;
           end (* else *)

     //*************************
     // output non-zero value   
     //*************************

     else
       begin
         if V < 0.0 then
           begin
             MINUS := TRUE ;
             V := - V
           end (* then *) ;
         FERTIG := FALSE ;
         EXPONENT := 0 ;
         WRITELN ( 'v = ' , V : 12 : 6 , ' expo = ' , EXPONENT : 1 ) ;
         while not FERTIG do
           begin
             if V >= 100000000.0 then
               begin
                 V := V / 100000000.0 ;
                 EXPONENT := EXPONENT + 8
               end (* then *)
             else
               if V < 0.000000001 then
                 begin
                   V := V * 100000000.0 ;
                   EXPONENT := EXPONENT - 8
                 end (* then *)
               else
                 if V >= 10000.0 then
                   begin
                     V := V / 10000.0 ;
                     EXPONENT := EXPONENT + 4
                   end (* then *)
                 else
                   if V < 0.00001 then
                     begin
                       V := V * 10000.0 ;
                       EXPONENT := EXPONENT - 4
                     end (* then *)
                   else
                     if V >= 10.0 then
                       begin
                         V := V / 10.0 ;
                         EXPONENT := EXPONENT + 1
                       end (* then *)
                     else
                       if V < 1.0 then
                         begin
                           V := V * 10.0 ;
                           EXPONENT := EXPONENT - 1
                         end (* then *)
                       else
                         FERTIG := TRUE
           end (* while *) ;
         WRITELN ( 'v = ' , V : 12 : 6 , ' expo = ' , EXPONENT : 1 ) ;
       end (* else *) ;

     //*********************************
     // add leading zeroes or blanks    
     // like in the integer procedure   
     //*********************************

     if MINUS then
       LEN := LEN + 1 ;
     if LEN < WIDTH then
       begin
         ZEROES := WIDTH - LEN ;
         if LEADZ then
           begin
             if MINUS then
               WRITE ( F , '-' ) ;
             for X := 1 to ZEROES do
               WRITE ( F , '0' ) ;
             if MINUS then
               WRITE ( F , BUFFER : LEN - 1 )
             else
               WRITE ( F , BUFFER : LEN ) ;
           end (* then *)
         else
           begin
             WRITE ( F , ' ' : ZEROES ) ;
             if MINUS then
               begin
                 WRITE ( F , '-' ) ;
                 WRITE ( F , BUFFER : LEN - 1 ) ;
               end (* then *)
             else
               WRITE ( F , BUFFER : LEN ) ;
           end (* else *)
       end (* then *)
     else
       $ERROR ( 1208 )
   end (* $PASWRR *) ;



procedure $PASWRX ( var F : TEXT ; V : INTEGER ; WIDTH : INTEGER ;
                  PMETA : ANYPTR ) ;

//**********************************************************
// write scalars to files                                   
// rewritten in Pascal - portable solution in 2020          
//**********************************************************
// Bernd Oppolzer - New Stanford Pascal                     
//**********************************************************


   type SHORT = 0 .. 32000 ;
        PSHORT = -> SHORT ;

   var PS : PSHORT ;
       LEFTX : BOOLEAN ;
       ELEMCOUNT : SHORT ;
       MAXIDLEN : SHORT ;
       MINIDLEN : SHORT ;
       LEN : SHORT ;
       SCAL_OFFS : SHORT ;
       CP : -> CHAR ;
       BUFFER : CHAR ( 20 ) ;

   begin (* $PASWRX *)
     if WIDTH = 0 then
       return ;

     //**********************************
     // access metadata for scalar type  
     //**********************************

     PS := PMETA ;
     if PS -> <> 0 then
       $ERROR ( 1220 ) ;
     PS := PTRADD ( PS , 2 ) ;
     ELEMCOUNT := PS -> ;
     PS := PTRADD ( PS , 2 ) ;
     MAXIDLEN := PS -> ;
     PS := PTRADD ( PS , 2 ) ;
     MINIDLEN := PS -> ;
     PS := PTRADD ( PS , 2 ) ;

     //**************
     // check width  
     //**************

     LEFTX := FALSE ;
     if WIDTH < 0 then
       begin
         LEFTX := TRUE ;
         WIDTH := - WIDTH ;
         if WIDTH = 1 then
           WIDTH := MAXIDLEN
       end (* then *) ;

     //************************************
     // error if width too small for type  
     //************************************

     if WIDTH < MINIDLEN then
       $ERROR ( 1221 ) ;

     //*******************************
     // error if scalar out of range  
     //*******************************

     if ( V < 0 ) or ( V >= ELEMCOUNT ) then
       $ERROR ( 1222 ) ;

     //*********************************
     // access string for scalar value  
     //*********************************

     PS := PTRADD ( PS , ( ELEMCOUNT - 1 - V ) * 4 ) ;
     SCAL_OFFS := PS -> ;
     PS := PTRADD ( PS , 2 ) ;
     LEN := PS -> ;
     BUFFER := ' ' ;
     CP := PTRADD ( PMETA , SCAL_OFFS ) ;
     MEMCPY ( ADDR ( BUFFER ) , CP , LEN ) ;
     if not LEFTX then
       WRITE ( F , SUBSTR ( BUFFER , 1 , LEN ) : WIDTH )
     else
       WRITE ( F , SUBSTR ( BUFFER , 1 , LEN ) : - WIDTH )
   end (* $PASWRX *) ;



begin (* HAUPTPROGRAMM *)
  
end (* HAUPTPROGRAMM *) .
