module $PASLIBX ;

(*********************************************************************)
(*$D+,A+                                                             *)
(*********************************************************************)
(*                                                                   *)
(*  Neue Speicherverwaltung fuer Pascal analog LE-Memory Manager     *)
(*                                                                   *)
(*  10.2016 - bernd.oppolzer@yahoo.com                               *)
(*                                                                   *)
(*********************************************************************)
(*                                                                   *)
(*  mit statischen Variablen - siehe static-Definitionen             *)
(*                                                                   *)
(*********************************************************************)



const HANCSIZE = 65536 ;
      TRLEVEL = 0 ;


type CHARPTR = -> CHAR ;
     CHAR128 = array [ 1 .. 128 ] of CHAR ;
     TOKEN = array [ 1 .. 8 ] of CHAR ;
     CHAR4 = array [ 1 .. 4 ] of CHAR ;
     CHAR8 = array [ 1 .. 8 ] of CHAR ;

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


static HEAPCB : HPCB ;
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

      begin (* DUMPCHAR *)
        if CH in [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' , 'A' .. 'I' ,
        'J' .. 'R' , 'S' .. 'Z' , '0' .. '9' , ' ' , ',' , '.' , '-' ,
        ';' , ':' , '_' , '!' , '"' , '�' , '$' , '%' , '&' , '/' , '('
        , ')' , '=' , '?' , '+' , '*' , '#' , '*' ] then
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
                 EXIT ( 1102 ) ;
               if PTRDIFF ( PEQUAL , PEQUALALT ) <= 0 then
                 EXIT ( 1103 ) ;
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
               EXIT ( 1101 ) ;
           end (* then *)
         else
           begin
             if PLAUF -> . FREELOW <> NIL then
               EXIT ( 1101 ) ;
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
       EXIT ( 1105 ) ;
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
       PLAUFF : PHFRE ;
       PFIRST : PHFRE ;
       SIZE_ALT : INTEGER ;
       VORG_MODUS : CHAR ;
       PLAUFALT : PHFRE ;
       SIZE_FREE : INTEGER ;
       SIZE_NEU : INTEGER ;
       ADRDIFF : INTEGER ;
       PLAUF_SUCH : PHFRE ;


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
            PLAUFX := HANC_ACT -> . FREE1 ;
          end (* else *) ;
        while TRUE do
          with PLAUFX -> do
            begin
              if FREELOW = NIL then
                break ;
              if SIZE_ALT <= SIZE_FREE then
                break ;
              if SIZE_ALT > 8 then
                SIZE_ALT := LEN_FREELOW
              else
                SIZE_ALT := 0 ;
              PLAUFALT := PLAUFX ;
              PLAUFX := FREELOW ;
            end (* with *) ;
        if PLAUFALT = NIL then
          EXIT ( 1111 ) ;
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
         WRITELN ( 'modify_tree: hanc_act = ' , HANC_ACT ) ;
         WRITELN ( 'modify_tree: size = ' , SIZE ) ;
         WRITELN ( 'modify_tree: plauf = ' , PLAUF ) ;
         WRITELN ( 'modify_tree: plaufact = ' , PLAUFACT ) ;
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

     /******************************************/
     /* sonderfall: free element ist immer     */
     /* noch groesser als das naechst tiefere  */
     /* ist auch dann der fall, wenn es        */
     /* gar kein tieferes element mehr gibt    */
     /******************************************/

                   if PLAUF -> . FREEEQ = NIL then
                     begin

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
                   PNACHB := PLAUF -> . FREEEQ ;
                   CHAIN_PRIOR ( PNACHB , VORG_MODUS , HANC_ACT ,
                                 PLAUFALT , PLAUF -> . LEN_FREEEQ ) ;
                   PNACHB -> . FREELOW := PLAUF -> . FREELOW ;
                   PNACHB -> . LEN_FREELOW := PLAUF -> . LEN_FREELOW ;
                 end (* then *)
               else
                 begin
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

               SUCHE_HFRE ;

     /****************************************/
     /* bei allen element vorher war         */
     /* LEN_FREELOW groesser; jetzt ist      */
     /* erstmals LEN_FREELOW kleiner gleich  */
     /* oder FREELOW nil                     */
     /****************************************/
     /* bei gleicher laenge:                 */
     /* einsortieren nach adressen           */
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
                   INS_HFRE_EQUAL ( PFREEN , SIZE_FREE , PLAUFALT -> .
                                    FREELOW , PLAUFALT , 'V' ) ;
                   return ;
                 end (* then *) ;

     /****************************************/
     /* andernfalls: der neue bereich        */
     /* muss ueber dem gefundenen            */
     /* einsortiert werden (neue laenge)     */
     /****************************************/

               PFREEN := PTRADD ( PLAUF , SIZE ) ;
               COPY_HFRE ( PLAUF , PFREEN , SIZE_FREE ) ;
               INS_HFRE_ABOVE ( PFREEN , SIZE , NIL , PLAUFALT , 'V' )
                                ;
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
                 EXIT ( 1100 ) ;

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
       EXIT ( 1104 ) ;

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

     if TRLEVEL >= 1 then
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
       EXIT ( 1106 ) ;
     with HANC_ACT -> do
       begin
         PTR_ENDE := PTRADD ( XP , SIZEX ) ;
         if TRLEVEL >= 3 then
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
           EXIT ( 1107 ) ;
         if PTRDIFF ( PTRADD ( AREA1 , LEN_AREA1 ) , PTR_ENDE ) < 0
         then
           EXIT ( 1108 ) ;
         if PTRDIFF ( AREA1 , XP ) > 0 then
           EXIT ( 1109 ) ;

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
                       EXIT ( 1110 ) ;

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
                   EXIT ( 1112 ) ;
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



function $PASMEM ( FUNCCODE : INTEGER ; X : VOIDPTR ; Y : VOIDPTR ) :
                 VOIDPTR ;

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
       otherwise
         $PASMEM := NIL
     end (* case *) ;
   end (* $PASMEM *) ;



function $PASLIB ( FUNCCODE : INTEGER ; X : VOIDPTR ; Y : VOIDPTR ) :
                 VOIDPTR ;

   begin (* $PASLIB *)
     if FUNCCODE = 10 then
       FUNCCODE := 7 ;
     $PASLIB := $PASMEM ( FUNCCODE , X , Y ) ;
   end (* $PASLIB *) ;





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


   var CH : CHAR ;

   begin (* $PASMAT *)
     case FUNCCODE of

     /*********************************/
     /* roundx                        */
     /*********************************/

       1 : begin
             $PASMAT := ROUNDX1 ( X , I ) ;
           end (* tag/ca *) ;
       otherwise
         EXIT ( 1120 ) ;
     end (* case *) ;
   end (* $PASMAT *) ;





(**********************************************************)
(*$D-,A+                                                  *)
(**********************************************************)
(*                                                        *)
(*  String-Handling Funktionen fuer Pascal                *)
(*                                                        *)
(*  11.2016 - bernd.oppolzer@yahoo.com                    *)
(*                                                        *)
(**********************************************************)
(*                                                        *)
(*  zunaechst nur MEMCPY und MEMSET                       *)
(*                                                        *)
(**********************************************************)




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



procedure $PASSTR ( FUNCCODE : INTEGER ; X1 : CHARPTR ; X2 : CHARPTR ;
                  L : INTEGER ) ;

(**************************************)
(*  Verteiler fuer String-Funktionen  *)
(**************************************)


   var CH : CHAR ;

   begin (* $PASSTR *)
     case FUNCCODE of

     /*********************************/
     /* MEMSET                        */
     /*********************************/

       1 : begin
             CH := CHR ( PTR2INT ( X2 ) ) ;
             while L > 0 do
               begin
                 X1 -> := CH ;
                 L := L - 1 ;
                 X1 := PTRADD ( X1 , 1 )
               end (* while *)
           end (* tag/ca *) ;

     /*********************************/
     /* MEMCPY                        */
     /*********************************/

       2 : begin
             while L > 0 do
               begin
                 X1 -> := X2 -> ;
                 L := L - 1 ;
                 X1 := PTRADD ( X1 , 1 ) ;
                 X2 := PTRADD ( X2 , 1 )
               end (* while *)
           end (* tag/ca *) ;
       otherwise
         EXIT ( 1120 ) ;
     end (* case *) ;
   end (* $PASSTR *) ;



begin (* HAUPTPROGRAMM *)

end (* HAUPTPROGRAMM *) .
