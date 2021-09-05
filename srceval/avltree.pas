module AVLTREE ;

(**************************************************************)
(*                                                            *)
(*   Portierung des AVLTREE Moduls nach Pascal                *)
(*   Soll funktionieren fuer beliebige AVL-Baeume             *)
(*   Key- und Datendefinition kommt "von aussen"              *)
(*                                                            *)
(*   Bernd Oppolzer - 07.2018                                 *)
(*                                                            *)
(**************************************************************)
(*$A+                                                         *)
(**************************************************************)



type PTR_AVLNODE = -> AVLNODE ;
     AVLNODE = record
                 MAGIC : CHAR ( 8 ) ;      // always 'AVLNODEX'
                 PVORG : PTR_AVLNODE ;     // Vorgaenger
                 PLN : PTR_AVLNODE ;       // linker Nachfolger
                 PRN : PTR_AVLNODE ;       // rechter Nachfolger
                 BALANCE : INTEGER ;       // balance lt. Wirth
                 KEY : VOIDPTR ;           // Schluessel
                 KEYLEN : INTEGER ;        // Keylen if deep copy
                 OBJ : VOIDPTR ;           // eigentliche Info
                 OBJLEN : INTEGER ;        // Objlen if deep copy
               end ;
     PTR_AVLLINK = -> AVLLINK ;
     AVLLINK = record                      // only used for avlprint
                 ZEICHNEN : CHAR ;         // storage for character
                 NEXT : PTR_AVLLINK ;      // to print the tree
                 PREV : PTR_AVLLINK ;      // linkage lines
               end ;
     PTR_AVLC = -> AVLC_ENTRY ;
     AVLC_ENTRY = record
                    MAGIC : CHAR ( 8 ) ;   // always 'AVLCACHE'
                    CNAME : CHAR ( 8 ) ;   // Name of Cache
                    COUNT : INTEGER ;      // Nbr of entries
                    PTREE : PTR_AVLNODE ;  // ptr to tree
                  end ;



local function AVLSRCH_INTERN ( SKEY : VOIDPTR ;         // look
                              SKEYLEN : INTEGER ;        // for
                              var POBJ : VOIDPTR ;       // comments
                              var POBJLEN : VOIDPTR ;    // at
                              var GEFUNDEN : BOOLEAN ;   // function
                              var PP : PTR_AVLNODE ;     // AVLSRCH
                              var HCHANGED : BOOLEAN ;   // below
                              EINFUEGEN : BOOLEAN ;      //
                              MODUS : CHAR ;             //
                              function AVLCOMP           // look
                              ( X1 : VOIDPTR ;           // for
                              L1 : INTEGER ;             // comments
                              X2 : VOIDPTR ;             // at
                              L2 : INTEGER )             // function
                              : INTEGER )                // AVLSRCH
                              : PTR_AVLNODE ;            // below

(********************************************************************)
(*                                                                  *)
(*   Suchfunktion im AVL-Baum (fuegt auch ein, falls noetig)        *)
(*                                                                  *)
(*   SKEY = Zeiger auf Suchargument fuer Suche und ggf. Einfg.      *)
(*   SKEYLEN = deep copy, wenn skeylen > 0 - dann wird der Key      *)
(*             beim Einfuegen in den AVL-Baum kopiert (ALLOC)       *)
(*             ansonsten wird nur der Pointer gespeichert           *)
(*   POBJ = pointer to OBJ is returned from tree node               *)
(*   POBJLEN = pointer to OBJLEN field is returned from tree node   *)
(*   GEFUNDEN = if true then found, otherwise inserted or NIL ret.  *)
(*   PP = actual AVL-tree, may be changed on return                 *)
(*   HCHANGED = init with false, may change during recursion        *)
(*   EINFUEGEN = set to true, if insert on not found condition      *)
(*   MODUS = type of search (= search or > search)                  *)
(*   AVLCOMP = Vergleichsfunktion fuer Suchargumente, muss          *)
(*             Werte zurueckgeben wie memcmp (1, -1, 0)             *)
(*                                                                  *)
(*   The AVL tree may be used to record pointers only; in this      *)
(*   case KEYLEN and OBJLEN are zero. Or it may be use to record    *)
(*   the contents as well. In this case, SKEYLEN should be          *)
(*   specified as a positive value. On insert, the key value        *)
(*   is copied into the AVL tree (using ALLOC and MEMCPY),          *)
(*   and on free, the storage is freed.                             *)
(*                                                                  *)
(*   Same goes for POBJ and POBJLEN, but AVLSRCH does not do        *)
(*   anything to the OBJ fields; it simply returns their            *)
(*   addresses and leaves it up to the caller to enter              *)
(*   the values and the length there. When freeing the tree,        *)
(*   the obj values are freed by AVLFREE, if objlen                 *)
(*   contains a nonzero value.                                      *)
(*                                                                  *)
(*   The AVLCOMP function gets the lengths of the two operands      *)
(*   as parameters, but with a pointer-only AVL tree, the           *)
(*   length parameters will be zero, and the AVLCOMP is             *)
(*   supposed to know the length in this case, anyway, and          *)
(*   to do the comparison correctly (maybe constant length).        *)
(*                                                                  *)
(********************************************************************)


   var P1 : PTR_AVLNODE ;
       P2 : PTR_AVLNODE ;
       P : PTR_AVLNODE ;
       PX : PTR_AVLNODE ;
       PRES : PTR_AVLNODE ;
       PVORG_SAVE : PTR_AVLNODE ;

   begin (* AVLSRCH_INTERN *)
     P := PP ;
     if P = NIL then
       begin
         GEFUNDEN := FALSE ;

     /************************************************************/
     /* Der Knoten existiert noch nicht, der gesuchte Schluessel */
     /* wurde also nicht gefunden.                               */
     /************************************************************/

         if EINFUEGEN then
           begin

     /************************************************************/
     /* Es wird ein neuer Knoten mit diesem Schluessel angelegt  */
     /* und in den Baum eingefuegt.                              */
     /************************************************************/

             P := ALLOC ( SIZEOF ( AVLNODE ) ) ;
             with P -> do
               begin
                 MAGIC := 'AVLNODEX' ;
                 PVORG := NIL ;
                 PLN := NIL ;
                 PRN := NIL ;
                 BALANCE := 0 ;
                 KEY := NIL ;
                 KEYLEN := 0 ;
                 OBJ := NIL ;
                 OBJLEN := 0 ;
               end (* with *) ;
             if SKEYLEN > 0 then
               begin

     /************************************************************/
     /* deep copy                                                */
     /************************************************************/

                 P -> . KEY := ALLOC ( SKEYLEN ) ;
                 MEMCPY ( P -> . KEY , SKEY , SKEYLEN ) ;
                 P -> . KEYLEN := SKEYLEN ;
               end (* then *)
             else
               P -> . KEY := SKEY ;
             HCHANGED := TRUE ;
             PRES := P ;
             POBJ := ADDR ( PRES -> . OBJ ) ;
             POBJLEN := ADDR ( PRES -> . OBJLEN ) ;
           end (* then *)
         else
           begin
             PRES := NIL ;
             POBJ := NIL ;
             POBJLEN := NIL ;
           end (* else *) ;
         PP := P ;
         AVLSRCH_INTERN := PRES ;
         return ;
       end (* then *) ;
     if AVLCOMP ( SKEY , SKEYLEN , P -> . KEY , P -> . KEYLEN ) < 0
     then
       begin

     /************************************************************/
     /* Der gesuchte Schluessel ist kleiner als der Schluessel   */
     /* des aktuellen Knotens. Es wird also im linken Teilbaum   */
     /* weitergesucht (rekursiver Aufruf). Nachdem das passiert  */
     /* ist, wird geprueft, ob sich der linke Teilbaum durch ein */
     /* eventuelles Einfuegen verlaengert hat.                   */
     /************************************************************/

         if P -> . PLN = NIL then

     /***********************************************************/
     /* new in 02.2020:                                         */
     /* if modus = '>', data is not returned                    */
     /* instead the key of the found entry (which may be        */
     /* greater than the requested key) is returned in the      */
     /* pobj fields. Another request is needed to retrieve      */
     /* the data.                                               */
     /***********************************************************/

           if MODUS = '>' then
             begin
               GEFUNDEN := TRUE ;
               PRES := P ;
               POBJ := ADDR ( PRES -> . KEY ) ;
               POBJLEN := ADDR ( PRES -> . KEYLEN ) ;
               if EINFUEGEN then
                 HCHANGED := FALSE ;
               PP := P ;
               AVLSRCH_INTERN := PRES ;
               return
             end (* then *) ;
         PRES := AVLSRCH_INTERN ( SKEY , SKEYLEN , POBJ , POBJLEN ,
                 GEFUNDEN , P -> . PLN , HCHANGED , EINFUEGEN , MODUS ,
                 AVLCOMP ) ;
         if EINFUEGEN and HCHANGED then
           begin
             PVORG_SAVE := P -> . PVORG ;

     /**************************************************/
     /* Falls der linke Teilbaum laenger geworden ist: */
     /**************************************************/

             case P -> . BALANCE of
               1 : begin

     /********************************************/
     /* bisher war der rechte Teilbaum laenger   */
     /********************************************/

                     P -> . BALANCE := 0 ;
                     HCHANGED := FALSE ;
                   end (* tag/ca *) ;
               0 : begin

     /*********************************************/
     /* bisher waren beide Teilbaeume gleich lang */
     /*********************************************/

                     P -> . BALANCE := - 1 ;
                   end (* tag/ca *) ;
               otherwise
                 begin

     /***************************************************/
     /* Der linke Teilbaum war ohnehin schon laenger.   */
     /* Jetzt muss der Baum umorganisiert werden!       */
     /* Zunaechst wird geprueft, ob beim linken Nach-   */
     /* folger der linke Teilbaum laenger ist (Fall A)  */
     /* oder der rechte (Fall B). Danach werden die     */
     /* Verbindungszeiger neu gesetzt.                  */
     /***************************************************/

                   P1 := P -> . PLN ;
                   if P1 -> . BALANCE = - 1 then
                     begin

     /************************************/
     /* Fall A                           */
     /************************************/

                       PX := P1 -> . PRN ;
                       P -> . PLN := PX ;
                       if PX <> NIL then
                         PX -> . PVORG := P ;
                       P1 -> . PRN := P ;
                       P -> . PVORG := P1 ;
                       P -> . BALANCE := 0 ;
                       P := P1 ;
                     end (* then *)
                   else
                     begin

     /************************************/
     /* Fall B                           */
     /************************************/

                       P2 := P1 -> . PRN ;
                       PX := P2 -> . PLN ;
                       P1 -> . PRN := PX ;
                       if PX <> NIL then
                         PX -> . PVORG := P1 ;
                       P2 -> . PLN := P1 ;
                       P1 -> . PVORG := P2 ;
                       PX := P2 -> . PRN ;
                       P -> . PLN := PX ;
                       if PX <> NIL then
                         PX -> . PVORG := P ;
                       P2 -> . PRN := P ;
                       P -> . PVORG := P2 ;
                       if P2 -> . BALANCE = - 1 then
                         P -> . BALANCE := 1
                       else
                         P -> . BALANCE := 0 ;
                       if P2 -> . BALANCE = 1 then
                         P1 -> . BALANCE := - 1
                       else
                         P1 -> . BALANCE := 0 ;
                       P := P2 ;
                     end (* else *) ;
                   P -> . BALANCE := 0 ;
                   HCHANGED := FALSE ;
                 end (* otherw *)
             end (* case *) ;
             if P -> . PLN <> NIL then
               P -> . PLN -> . PVORG := P ;
             P -> . PVORG := PVORG_SAVE ;
           end (* then *)
       end (* then *)
     else
       if AVLCOMP ( SKEY , SKEYLEN , P -> . KEY , P -> . KEYLEN ) > 0
       then
         begin

     /************************************************************/
     /* Der gesuchte Schluessel ist groesser als der Schluessel  */
     /* des aktuellen Knotens. Es wird also im rechten Teilbaum  */
     /* weitergesucht (rekursiver Aufruf). Nachdem das passiert  */
     /* ist, wird geprueft ob sich der rechte Teilbaum durch ein */
     /* eventuelles Einfuegen verlaengert hat.                   */
     /************************************************************/

           PRES := AVLSRCH_INTERN ( SKEY , SKEYLEN , POBJ , POBJLEN ,
                   GEFUNDEN , P -> . PRN , HCHANGED , EINFUEGEN , MODUS
                   , AVLCOMP ) ;
           if EINFUEGEN and HCHANGED then
             begin
               PVORG_SAVE := P -> . PVORG ;

     /***************************************************/
     /* Falls der rechte Teilbaum laenger geworden ist: */
     /***************************************************/

               case P -> . BALANCE of
                 - 1 : begin

     /********************************************/
     /* bisher war der linke Teilbaum laenger    */
     /********************************************/

                         P -> . BALANCE := 0 ;
                         HCHANGED := FALSE ;
                       end (* tag/ca *) ;
                 0 : begin

     /*********************************************/
     /* bisher waren beide Teilbaeume gleich lang */
     /*********************************************/

                       P -> . BALANCE := 1 ;
                     end (* tag/ca *) ;
                 otherwise
                   begin

     /***************************************************/
     /* Der rechte Teilbaum war ohnehin schon laenger.  */
     /* Jetzt muss der Baum umorganisiert werden!       */
     /* Zunaechst wird geprueft, ob beim rechten Nach-  */
     /* folger der rechte Teilbaum laenger ist (Fall A) */
     /* oder der linke (Fall B). Danach werden die      */
     /* Verbindungszeiger neu gesetzt.                  */
     /***************************************************/

                     P1 := P -> . PRN ;
                     if P1 -> . BALANCE = 1 then
                       begin

     /************************************/
     /* Fall A                           */
     /************************************/

                         PX := P1 -> . PLN ;
                         P -> . PRN := PX ;
                         if PX <> NIL then
                           PX -> . PVORG := P ;
                         P1 -> . PLN := P ;
                         P -> . PVORG := P1 ;
                         P -> . BALANCE := 0 ;
                         P := P1 ;
                       end (* then *)
                     else
                       begin

     /************************************/
     /* Fall B                           */
     /************************************/

                         P2 := P1 -> . PLN ;
                         PX := P2 -> . PRN ;
                         P1 -> . PLN := PX ;
                         if PX <> NIL then
                           PX -> . PVORG := P1 ;
                         P2 -> . PRN := P1 ;
                         P1 -> . PVORG := P2 ;
                         PX := P2 -> . PLN ;
                         P -> . PRN := PX ;
                         if PX <> NIL then
                           PX -> . PVORG := P ;
                         P2 -> . PLN := P ;
                         P -> . PVORG := P2 ;
                         if P2 -> . BALANCE = 1 then
                           P -> . BALANCE := - 1
                         else
                           P -> . BALANCE := 0 ;
                         if P2 -> . BALANCE = - 1 then
                           P1 -> . BALANCE := 1
                         else
                           P1 -> . BALANCE := 0 ;
                         P := P2 ;
                       end (* else *) ;
                     P -> . BALANCE := 0 ;
                     HCHANGED := FALSE ;
                   end (* otherw *)
               end (* case *) ;
               if P -> . PRN <> NIL then
                 P -> . PRN -> . PVORG := P ;
               P -> . PVORG := PVORG_SAVE ;
             end (* then *)
         end (* then *)
       else
         begin
           GEFUNDEN := TRUE ;

     /***********************************************************/
     /* Schluessel gefunden, diesen Knoten zurueckgeben         */
     /***********************************************************/

           PRES := P ;
           POBJ := ADDR ( PRES -> . OBJ ) ;
           POBJLEN := ADDR ( PRES -> . OBJLEN ) ;
           if EINFUEGEN then
             HCHANGED := FALSE ;
         end (* else *) ;
     PP := P ;
     AVLSRCH_INTERN := PRES ;
   end (* AVLSRCH_INTERN *) ;



function AVLSRCH ( SKEY : VOIDPTR ;        // ptr to key
                 SKEYLEN : INTEGER ;       // keylen (if deep copy)
                 var POBJ : VOIDPTR ;      // ptr to obj ptr
                 var POBJLEN : VOIDPTR ;   // ptr to objlen field
                 var GEFUNDEN : BOOLEAN ;  // true if found
                 var PP : PTR_AVLNODE ;    // tree pointer
                 var HCHANGED : BOOLEAN ;  // height changed
                 EINFUEGEN : BOOLEAN ;     // if insert then true
                 function AVLCOMP          // passed as parameter:
                 ( X1 : VOIDPTR ;          // compare func for
                 L1 : INTEGER ;            // nodes in avl tree
                 X2 : VOIDPTR ;            // integer return values
                 L2 : INTEGER )            // like memcmp
                 : INTEGER ) : PTR_AVLNODE ;

   begin (* AVLSRCH *)
     if PP <> NIL then
       if PP -> . MAGIC <> 'AVLNODEX' then
         begin
           WRITELN (
              '+++ AVLSRCH: parameter PP invalid (MAGIC field not ok)'
                     ) ;
           EXIT ( 2001 ) ;
         end (* then *) ;
     AVLSRCH := AVLSRCH_INTERN ( SKEY , SKEYLEN , POBJ , POBJLEN ,
                GEFUNDEN , PP , HCHANGED , EINFUEGEN , '=' , AVLCOMP )
                ;
   end (* AVLSRCH *) ;



function AVLSRCHX ( SKEY : VOIDPTR ;        // ptr to key
                  SKEYLEN : INTEGER ;       // keylen (if deep copy)
                  var POBJ : VOIDPTR ;      // ptr to obj ptr
                  var POBJLEN : VOIDPTR ;   // ptr to objlen field
                  var GEFUNDEN : BOOLEAN ;  // true if found
                  var PP : PTR_AVLNODE ;    // tree pointer
                  var HCHANGED : BOOLEAN ;  // height changed
                  EINFUEGEN : BOOLEAN ;     // if insert then true
                  MODUS : CHAR ;            // = or > (type of search)
                  function AVLCOMP          // passed as parameter:
                  ( X1 : VOIDPTR ;          // compare func for
                  L1 : INTEGER ;            // nodes in avl tree
                  X2 : VOIDPTR ;            // integer return values
                  L2 : INTEGER )            // like memcmp
                  : INTEGER ) : PTR_AVLNODE ;

   begin (* AVLSRCHX *)
     if PP <> NIL then
       if PP -> . MAGIC <> 'AVLNODEX' then
         begin
           WRITELN (
              '+++ AVLSRCH: parameter PP invalid (MAGIC field not ok)'
                     ) ;
           EXIT ( 2001 ) ;
         end (* then *) ;
     AVLSRCHX := AVLSRCH_INTERN ( SKEY , SKEYLEN , POBJ , POBJLEN ,
                 GEFUNDEN , PP , HCHANGED , EINFUEGEN , MODUS , AVLCOMP
                 ) ;
   end (* AVLSRCHX *) ;



function AVLGET ( MODUS : CHAR ;           // mode = F(irst), N(ext)
                START : PTR_AVLNODE ;      // starting position
                var RESULT : PTR_AVLNODE ; // new position
                var PKEY : VOIDPTR ;       // pointer to key
                var KEYLEN : INTEGER ;     // keylen
                var POBJ : VOIDPTR ;       // pointer to obj
                var OBJLEN : INTEGER )     // objlen
                : INTEGER ;                // zero, if OK

(********************************************************************)
(*                                                                  *)
(*   suche nachfolger im baum:                                      *)
(*                                                                  *)
(*   vorwaerts:                                                     *)
(*                                                                  *)
(*   wenn rechter nachfolger da,                                    *)
(*      gehe dorthin, und                                           *)
(*      dann immer linker nachfolger, bis keiner mehr da ist.       *)
(*      fertig                                                      *)
(*                                                                  *)
(*   wenn kein rechter nachfolger da:                               *)
(*      wenn kein vorgaenger da: ende der iteration                 *)
(*      wenn vorgaenger da:                                         *)
(*         ist aktueller knoten linker nachfolger des vorgaengers?  *)
(*         ja: vorgaenger nehmen und fertig.                        *)
(*         nein: weiter mit vorgaenger                              *)
(*                                                                  *)
(*   rueckwaerts:                                                   *)
(*                                                                  *)
(*   wenn linker nachfolger da,                                     *)
(*      gehe dorthin, und                                           *)
(*      dann immer rechter nachfolger, bis keiner mehr da ist.      *)
(*      fertig                                                      *)
(*                                                                  *)
(*   wenn kein linker nachfolger da:                                *)
(*      wenn kein vorgaenger da: ende der iteration                 *)
(*      wenn vorgaenger da:                                         *)
(*         ist aktueller knoten rechter nachfolger des vorgaengers? *)
(*         ja: vorgaenger nehmen und fertig.                        *)
(*         nein: weiter mit vorgaenger                              *)
(*                                                                  *)
(*   erster und letzter ist einfach:                                *)
(*                                                                  *)
(*   nach oben und dann ganz links oder ganz rechts runter          *)
(*                                                                  *)
(*------------------------------------------------------------------*)
(*                                                                  *)
(*   Die folgende Funktion navigiert auf dem Baum;                  *)
(*   dabei kann wie ueblich first, last, next, previous             *)
(*   mitgegeben werden (F,L,N,P).                                   *)
(*                                                                  *)
(*   Fuer start muss irgendein Knoten des Baums mitgegeben          *)
(*   werden (bei F und L geht es am schnellsten mit dem             *)
(*   root-Knoten, aber alle anderen gehen auch). Bei N und P        *)
(*   wird der jeweils naechste bzw. vorhergehende zurueck-          *)
(*   gegeben, ansonsten der erste bzw. letzte ueberhaupt.           *)
(*                                                                  *)
(*------------------------------------------------------------------*)
(*                                                                  *)
(*   erstellt 12.2009 - OPP                                         *)
(*                                                                  *)
(********************************************************************)


   var RES : PTR_AVLNODE ;
       P : PTR_AVLNODE ;
       RC : INTEGER ;
       RESCHECK : PTR_AVLNODE ;
       CHECK_RESULT : BOOLEAN ;

   begin (* AVLGET *)
     RES := NIL ;
     PKEY := NIL ;
     POBJ := NIL ;
     KEYLEN := - 1 ;
     OBJLEN := - 1 ;
     CHECK_RESULT := FALSE ;
     RC := 0 ;
     if START = NIL then
       RC := 2
     else
       if START -> . MAGIC <> 'AVLNODEX' then
         begin
           WRITELN (
            '+++ AVLGET: parameter START invalid (MAGIC field not ok)'
                     ) ;
           EXIT ( 2001 ) ;
         end (* then *) ;
     if RC = 0 then
       begin
         while START -> . PVORG <> NIL do
           START := START -> . PVORG ;
         if MODUS = 'N' then
           begin
             if RESULT = NIL then
               MODUS := 'F'
             else
               CHECK_RESULT := TRUE
           end (* then *) ;
         if MODUS = 'P' then
           begin
             if RESULT = NIL then
               MODUS := 'L'
             else
               CHECK_RESULT := TRUE
           end (* then *)
       end (* then *) ;
     if CHECK_RESULT then
       begin
         if RESULT -> . MAGIC <> 'AVLNODEX' then
           begin
             WRITELN (
           '+++ AVLGET: parameter RESULT invalid (MAGIC field not ok)'
                       ) ;
             EXIT ( 2001 ) ;
           end (* then *) ;
         RESCHECK := RESULT ;
         while RESCHECK -> . PVORG <> NIL do
           RESCHECK := RESCHECK -> . PVORG ;
         if RESCHECK <> START then
           RC := 3
       end (* then *) ;
     case MODUS of
       'F' : if RC = 0 then
               begin
                 while START -> . PLN <> NIL do
                   START := START -> . PLN ;
                 RES := START ;
               end (* then *) ;
       'L' : if RC = 0 then
               begin
                 while START -> . PRN <> NIL do
                   START := START -> . PRN ;
                 RES := START ;
               end (* then *) ;
       'N' : if RC = 0 then
               begin
                 P := RESULT -> . PRN ;
                 if P <> NIL then
                   begin
                     while P -> . PLN <> NIL do
                       P := P -> . PLN ;
                     RES := P ;
                   end (* then *)
                 else
                   begin
                     P := RESULT ;
                     while TRUE do
                       begin
                         if P -> . PVORG = NIL then
                           begin
                             RC := 1 ;
                             break ;
                           end (* then *) ;
                         if P = P -> . PVORG -> . PLN then
                           begin
                             RES := P -> . PVORG ;
                             break ;
                           end (* then *) ;
                         P := P -> . PVORG ;
                       end (* while *)
                   end (* else *)
               end (* then *) ;
       'P' : if RC = 0 then
               begin
                 P := RESULT -> . PLN ;
                 if P <> NIL then
                   begin
                     while P -> . PRN <> NIL do
                       P := P -> . PRN ;
                     RES := P ;
                   end (* then *)
                 else
                   begin
                     P := RESULT ;
                     while TRUE do
                       begin
                         if P -> . PVORG = NIL then
                           begin
                             RC := 1 ;
                             break ;
                           end (* then *) ;
                         if P = P -> . PVORG -> . PRN then
                           begin
                             RES := P -> . PVORG ;
                             break ;
                           end (* then *) ;
                         P := P -> . PVORG ;
                       end (* while *)
                   end (* else *)
               end (* then *) ;
       otherwise
         begin
           RC := - 1 ;
         end (* otherw *)
     end (* case *) ;
     RESULT := RES ;
     if RES <> NIL then
       begin
         PKEY := RES -> . KEY ;
         KEYLEN := RES -> . KEYLEN ;
         POBJ := RES -> . OBJ ;
         OBJLEN := RES -> . OBJLEN ;
       end (* then *) ;
     AVLGET := RC ;
   end (* AVLGET *) ;



procedure AVLPRINT ( P : PTR_AVLNODE ;     // tree to print
                   var AUSGFILE : TEXT ;   // output file
                   EINRUECK : INTEGER ;    // indentation count
                   PV : PTR_AVLLINK ;      // nil on top level call
                   RICHTUNG : CHAR ;       // blank on top level call
                   procedure AVLPKEY       // passed as parameter:
                   ( var F : TEXT ;        // procedure to print
                   P : VOIDPTR ) ) ;       // one key value

(********************************************************************)
(*                                                                  *)
(*   Drucken AVL-Baum                                               *)
(*                                                                  *)
(*   P = Zeiger auf AVL-Baum                                        *)
(*   AUSGFILE = Textfile, auf den gedruckt werden soll              *)
(*   EINRUECK = Anzahl Einrueckzeichen pro Ebene                    *)
(*   AVLPKEY = Prozedur zum Drucken eines Knotens                   *)
(*   PV = Verbindungsinformation, bei Top Level NIL mitgeben        *)
(*   RICHTUNG = Teilbauminfo, bei Top Level Blank mitgeben          *)
(*                                                                  *)
(********************************************************************)


   const SW = '-' ;
         SS = '|' ;

   var I : INTEGER ;
       VN : AVLLINK ;
       PVL : PTR_AVLLINK ;

   static PVFIRST : PTR_AVLLINK ;

   begin (* AVLPRINT *)
     if P <> NIL then
       begin

     /*********************************************/
     /*   Rechten Teilbaum ausgeben. Dazu         */
     /*   Verbindungsstruktur an die              */
     /*   verkettete Liste dranhaengen            */
     /*********************************************/

         if PV <> NIL then
           PV -> . NEXT := ADDR ( VN )
         else
           PVFIRST := ADDR ( VN ) ;
         VN . PREV := PV ;
         VN . ZEICHNEN := 'N' ;
         VN . NEXT := NIL ;
         AVLPRINT ( P -> . PRN , AUSGFILE , EINRUECK , ADDR ( VN ) ,
                    'R' , AVLPKEY ) ;

     /*********************************************************/
     /*   Schreiben der Key-Information und evtl. not-        */
     /*   wendiger Verbindungszeichen                         */
     /*********************************************************/

         if PV <> NIL then
           begin
             WRITE ( AUSGFILE , ' ' : EINRUECK ) ;
             PVL := PVFIRST ;
             while PVL <> PV do
               begin
                 if PVL -> . ZEICHNEN = 'J' then
                   WRITE ( AUSGFILE , SS : EINRUECK )
                 else
                   WRITE ( AUSGFILE , ' ' : EINRUECK ) ;
                 PVL := PVL -> . NEXT
               end (* while *)
           end (* then *) ;

     //************************************************************
     //  if (p -> pvorg <> NIL)                                    
     //     printf ("%05d/", p -> pvorg -> key);                   
     //  else                                                      
     //     printf (" NIL/");                                      
     //************************************************************

         AVLPKEY ( AUSGFILE , P -> . KEY ) ;
         if PV <> NIL then
           if RICHTUNG = 'R' then
             PV -> . ZEICHNEN := 'J'
           else
             PV -> . ZEICHNEN := 'N' ;
         if P -> . PLN <> NIL then
           VN . ZEICHNEN := 'J'
         else
           VN . ZEICHNEN := 'N' ;
         if ( P -> . PRN <> NIL ) or ( P -> . PLN <> NIL ) then
           begin

     /********************************************************/
     /*   Falls es Nachfolge-Knoten gibt, horizontale        */
     /*   Verbindungszeichen ausgeben                        */
     /********************************************************/

             WRITE ( AUSGFILE , ' ' ) ;
             for I := 1 to EINRUECK - 2 do
               WRITE ( AUSGFILE , SW ) ;
             WRITE ( AUSGFILE , SS ) ;
           end (* then *) ;
         WRITELN ( AUSGFILE ) ;

     /*******************************************************/
     /*   Verbindungszeichen ausgeben                       */
     /*******************************************************/

         if FALSE then
           begin
             VN . NEXT := NIL ;
             WRITE ( AUSGFILE , ' ' : EINRUECK ) ;
             PVL := PVFIRST ;
             while PVL <> PV do
               begin
                 if PVL -> . ZEICHNEN = 'J' then
                   WRITE ( AUSGFILE , SS : EINRUECK )
                 else
                   WRITE ( AUSGFILE , ' ' : EINRUECK ) ;
                 PVL := PVL -> . NEXT
               end (* while *) ;
             WRITELN ( AUSGFILE ) ;
           end (* then *) ;

     /*********************************/
     /*   Linken Teilbaum ausgeben    */
     /*********************************/

         AVLPRINT ( P -> . PLN , AUSGFILE , EINRUECK , ADDR ( VN ) ,
                    'L' , AVLPKEY )
       end (* then *)
   end (* AVLPRINT *) ;



procedure AVLFREE ( P : PTR_AVLNODE ) ;

   begin (* AVLFREE *)
     if P <> NIL then
       begin
         AVLFREE ( P -> . PLN ) ;
         AVLFREE ( P -> . PRN ) ;
         if P -> . KEYLEN > 0 then
           if P -> . KEY <> NIL then
             FREE ( P -> . KEY ) ;
         if P -> . OBJLEN > 0 then
           if P -> . OBJ <> NIL then
             FREE ( P -> . OBJ ) ;
         FREE ( P ) ;
       end (* then *)
   end (* AVLFREE *) ;



function AVLCACHE ( FUNKCODE : CHAR ( 8 ) ;   // Funktionscode
                  PHANDLE : VOIDPTR ;         // Cachehandle
                  var SEQKEY : VOIDPTR ;      // seq. Keyposition,
                  var PKEY : VOIDPTR ;        // Zeiger auf Key
                  var LKEY : INTEGER ;        // Laenge Key
                  var PDAT : VOIDPTR ;        // Zeiger auf Daten
                  var LDAT : INTEGER )        // Laenge Daten
                  : INTEGER ;

(********************************************************************)
(*                                                                  *)
(*   Create and manage Cache Areas using AVL-Trees                  *)
(*                                                                  *)
(*   FUNKCODE = function code, see below                            *)
(*   PHANDLE = Cache Handle (once the cache has been created)       *)
(*   SEQKEY = used when processing a cache sequentially             *)
(*   PKEY = pointer to cache key                                    *)
(*   LKEY = length of cache key                                     *)
(*   PDAT = pointer to cache data                                   *)
(*   LDAT = length of cache data                                    *)
(*                                                                  *)
(*   Function CREATE (also used to locate existing cache):          *)
(*                                                                  *)
(*   PKEY points to 8 byte cache name                               *)
(*   LKEY is not used                                               *)
(*                                                                  *)
(*   if successful, PDAT returns cache handle                       *)
(*   LDAT returns 20 (= current sizeof cache handle)                *)
(*   function result is zero, when (empty) cache is created         *)
(*   4, when existing cache has been located,                       *)
(*   other return codes are errors                                  *)
(*                                                                  *)
(*   Function GET:                                                  *)
(*                                                                  *)
(*   PHANDLE: handle for cache (returned by CREATE)                 *)
(*   PKEY / LKEY: identifies key                                    *)
(*   PDAT / LDAT: to return the data, if key found                  *)
(*                                                                  *)
(*   function result is zero, when data is found                    *)
(*   and 8, if data is not found;                                   *)
(*   20, if phandle is NIL or invalid                               *)
(*                                                                  *)
(*   Function PUT:                                                  *)
(*                                                                  *)
(*   PHANDLE: handle for cache (returned by CREATE)                 *)
(*   PKEY / LKEY: identifies key                                    *)
(*   PDAT / LDAT: identifies data                                   *)
(*                                                                  *)
(*   function result is zero, when data is inserted into            *)
(*   cache, and 4, when data is replaced (freed and inserted);      *)
(*   20, if phandle is NIL or invalid                               *)
(*                                                                  *)
(*   Function GFIRST:                                               *)
(*                                                                  *)
(*   PHANDLE: handle for cache (returned by CREATE)                 *)
(*   SEQKEY: stores the cache reading position between calls        *)
(*   PKEY / LKEY: to return the key, if entry found                 *)
(*   PDAT / LDAT: to return the data, if entry found                *)
(*                                                                  *)
(*   function result is zero, when data is found                    *)
(*   and 8, if data is not found (empty cache)                      *)
(*   20, if phandle is NIL or invalid                               *)
(*                                                                  *)
(*   Function GNEXT:                                                *)
(*                                                                  *)
(*   PHANDLE: handle for cache (returned by CREATE)                 *)
(*   SEQKEY: stores the cache reading position between calls        *)
(*   PKEY / LKEY: to return the key, if entry found                 *)
(*   PDAT / LDAT: to return the data, if entry found                *)
(*                                                                  *)
(*   function result is zero, when data is found                    *)
(*   and 8, if data is not found (no next cache entry)              *)
(*   20, if phandle is NIL or invalid                               *)
(*                                                                  *)
(*   note: calling GNEXT with a SEQKEY of NIL is the same           *)
(*   as calling GFIRST                                              *)
(*                                                                  *)
(*   Function TRACE:                                                *)
(*                                                                  *)
(*   PHANDLE: handle for cache (returned by CREATE)                 *)
(*   PKEY: should be NIL or point to an 8 byte cache name           *)
(*   all other parameters have no meaning                           *)
(*                                                                  *)
(*   the cache is printed; the key and data is treated              *)
(*   as characters strings of the stored length                     *)
(*                                                                  *)
(*   function result is zero, when data is found                    *)
(*   and 8, if data is not found (empty cache)                      *)
(*   20, if phandle is NIL or invalid                               *)
(*                                                                  *)
(********************************************************************)


   type PVOIDPTR = -> VOIDPTR ;

   var CMDX : INTEGER ;
       RC : INTEGER ;
       I : INTEGER ;
       PCACHENAME : -> CHAR ( 8 ) ;
       PPAVLC : -> PTR_AVLC ;
       PAVLC : PTR_AVLC ;
       POBJ : PVOIDPTR ;
       POBJLEN : -> INTEGER ;
       PKEYNEU : PVOIDPTR ;
       LKEYNEU : -> INTEGER ;
       PLEN : -> INTEGER ;
       PRES : VOIDPTR ;
       GEFUNDEN : BOOLEAN ;
       HCHANGED : BOOLEAN ;
       PBAUMX : VOIDPTR ;
       RCFOUND : INTEGER ;
       PKEY_LOCAL : VOIDPTR ;
       LKEY_LOCAL : INTEGER ;
       PDAT_LOCAL : VOIDPTR ;
       LDAT_LOCAL : INTEGER ;
       SEQKEY_LOCAL : VOIDPTR ;
       PC254 : -> CHAR ( 254 ) ;

   static PCACHEDIR : VOIDPTR ;

   const COMMAND_COUNT = 10 ;
         COMMANDS : array [ 1 .. COMMAND_COUNT ] of CHAR ( 8 ) =
         ( 'CREATE' , 'GET' , 'PUT' , 'GFIRST' , 'GNEXT' , 'TRACE' ,
           'DELETE' , 'SHOWALL' , 'START' , 'CLEAR' ) ;


   function DIRCOMP ( X1 : VOIDPTR ; L1 : INTEGER ; X2 : VOIDPTR ; L2 :
                    INTEGER ) : INTEGER ;

   //****************************************************************
   // compare function for AVL tree key values (here: char (8) keys) 
   // this function is passed as a parameter to avlsrch              
   //****************************************************************


      var S1 : CHAR ( 8 ) ;
          SP1 : -> CHAR ( 8 ) ;
          S2 : CHAR ( 8 ) ;
          SP2 : -> CHAR ( 8 ) ;

      begin (* DIRCOMP *)

        //**********************************************
        // this coding simply to avoid warning message  
        // about parameters not used :-)                
        //**********************************************

        if FALSE then
          if L1 <> L2 then
            begin
              L1 := 8 ;
              L2 := 8
            end (* then *) ;
        SP1 := X1 ;
        SP2 := X2 ;
        S1 := SP1 -> ;
        S2 := SP2 -> ;
        if S1 > S2 then
          DIRCOMP := 1
        else
          if S1 < S2 then
            DIRCOMP := - 1
          else
            DIRCOMP := 0 ;
      end (* DIRCOMP *) ;


   function CACHECOMP ( X1 : VOIDPTR ; L1 : INTEGER ; X2 : VOIDPTR ; L2
                      : INTEGER ) : INTEGER ;

   //****************************************************************
   // compare function for AVL tree key values                       
   // here: arbitrary structures of varying length                   
   // this function is passed as a parameter to avlsrch              
   //****************************************************************


      begin (* CACHECOMP *)
        if L1 > L2 then
          CACHECOMP := 1
        else
          if L1 < L2 then
            CACHECOMP := - 1
          else
            CACHECOMP := MEMCMP ( X1 , X2 , L1 ) ;
      end (* CACHECOMP *) ;


   begin (* AVLCACHE *)
     CMDX := 0 ;
     for I := 1 to COMMAND_COUNT do
       if FUNKCODE = COMMANDS [ I ] then
         begin
           CMDX := I ;
           break
         end (* then *) ;
     case CMDX of
       1 : begin  // CREATE (or LOCATE)
             PDAT := NIL ;
             LDAT := 0 ;
             PCACHENAME := PKEY ;
             HCHANGED := FALSE ;
             PRES := AVLSRCH ( PCACHENAME , 8 , PPAVLC , PLEN ,
                     GEFUNDEN , PCACHEDIR , HCHANGED , TRUE , DIRCOMP )
                     ;
             if PRES = NIL then
               RC := 12
             else
               begin
                 if GEFUNDEN then
                   begin
                     PAVLC := PPAVLC -> ;
                     RC := 4
                   end (* then *)
                 else
                   begin
                     RC := 0 ;
                     PPAVLC -> := ALLOC ( SIZEOF ( AVLC_ENTRY ) ) ;
                     PAVLC := PPAVLC -> ;
                     with PAVLC -> do
                       begin
                         MAGIC := 'AVLCACHE' ;
                         CNAME := PCACHENAME -> ;
                         COUNT := 0 ;
                         PTREE := NIL ;
                       end (* with *) ;
                     PLEN -> := SIZEOF ( AVLC_ENTRY ) ;
                   end (* else *) ;
                 PDAT := PAVLC ;
                 LDAT := PLEN -> ;
               end (* else *)
           end (* tag/ca *) ;
       2 : begin
             repeat  // GET - one time loop
               PAVLC := PHANDLE ;

     //************************************************************
     // check handle and return if handle nok                      
     //************************************************************

               if PAVLC = NIL then
                 begin
                   RC := 20 ;
                   break
                 end (* then *) ;
               if PAVLC -> . MAGIC <> 'AVLCACHE' then
                 begin
                   RC := 20 ;
                   break
                 end (* then *) ;

     //************************************************************
     // search for key and return data if found                    
     //************************************************************

               PBAUMX := PAVLC -> . PTREE ;
               HCHANGED := FALSE ;
               PRES := AVLSRCH ( PKEY , LKEY , POBJ , POBJLEN ,
                       GEFUNDEN , PBAUMX , HCHANGED , FALSE , CACHECOMP
                       ) ;
               if not GEFUNDEN then
                 begin
                   PDAT := NIL ;
                   LDAT := 0 ;
                   RC := 8 ;
                 end (* then *)
               else
                 begin
                   PDAT := POBJ -> ;
                   LDAT := POBJLEN -> ;
                   RC := 0 ;
                 end (* else *)
             until TRUE
           end (* tag/ca *) ;
       3 : begin
             repeat  // PUT - one time loop
               PAVLC := PHANDLE ;

     //************************************************************
     // check handle and return if handle nok                      
     //************************************************************

               if PAVLC = NIL then
                 begin
                   RC := 20 ;
                   break
                 end (* then *) ;
               if PAVLC -> . MAGIC <> 'AVLCACHE' then
                 begin
                   RC := 20 ;
                   break
                 end (* then *) ;

     //************************************************************
     // search for key and insert if notfound                      
     //************************************************************

               PBAUMX := PAVLC -> . PTREE ;
               HCHANGED := FALSE ;
               PRES := AVLSRCH ( PKEY , LKEY , POBJ , POBJLEN ,
                       GEFUNDEN , PBAUMX , HCHANGED , TRUE , CACHECOMP
                       ) ;
               if not GEFUNDEN then
                 begin

     //************************************************************
     // insert data if notfound                                    
     //************************************************************

                   PAVLC -> . COUNT := PAVLC -> . COUNT + 1 ;
                   POBJ -> := ALLOC ( LDAT ) ;
                   POBJLEN -> := LDAT ;
                   MEMCPY ( POBJ -> , PDAT , LDAT ) ;
                   PAVLC -> . PTREE := PBAUMX ;
                   RC := 0 ;
                 end (* then *)
               else
                 begin

     //************************************************************
     // replace data if notfound                                   
     //************************************************************

                   FREE ( POBJ -> ) ;
                   POBJ -> := ALLOC ( LDAT ) ;
                   POBJLEN -> := LDAT ;
                   MEMCPY ( POBJ -> , PDAT , LDAT ) ;
                   RC := 4 ;
                 end (* else *)
             until TRUE
           end (* tag/ca *) ;
       4 : begin
             repeat  // GFIRST - one time loop
               PAVLC := PHANDLE ;

     //************************************************************
     // check handle and return if handle nok                      
     //************************************************************

               if PAVLC = NIL then
                 begin
                   RC := 20 ;
                   break
                 end (* then *) ;
               if PAVLC -> . MAGIC <> 'AVLCACHE' then
                 begin
                   RC := 20 ;
                   break
                 end (* then *) ;

     //************************************************************
     // search for first key using AVLGET / F                      
     //************************************************************

               PBAUMX := PAVLC -> . PTREE ;
               RCFOUND := AVLGET ( 'F' , PBAUMX , SEQKEY , PKEY , LKEY
                          , PDAT , LDAT ) ;
               if RCFOUND <> 0 then
                 begin
                   PKEY := NIL ;
                   LKEY := 0 ;
                   PDAT := NIL ;
                   LDAT := 0 ;
                   RC := 8 ;
                 end (* then *)
               else
                 begin
                   RC := 0 ;
                 end (* else *)
             until TRUE
           end (* tag/ca *) ;
       5 : begin
             repeat  // GNEXT - one time loop
               PAVLC := PHANDLE ;

     //************************************************************
     // check handle and return if handle nok                      
     //************************************************************

               if PAVLC = NIL then
                 begin
                   RC := 20 ;
                   break
                 end (* then *) ;
               if PAVLC -> . MAGIC <> 'AVLCACHE' then
                 begin
                   RC := 20 ;
                   break
                 end (* then *) ;

     //************************************************************
     // search for next key using AVLGET / N                       
     //************************************************************

               PBAUMX := PAVLC -> . PTREE ;
               RCFOUND := AVLGET ( 'N' , PBAUMX , SEQKEY , PKEY , LKEY
                          , PDAT , LDAT ) ;
               if RCFOUND <> 0 then
                 begin
                   PKEY := NIL ;
                   LKEY := 0 ;
                   PDAT := NIL ;
                   LDAT := 0 ;
                   RC := 8 ;
                 end (* then *)
               else
                 begin
                   RC := 0 ;
                 end (* else *)
             until TRUE
           end (* tag/ca *) ;
       6 : begin
             repeat  // TRACE - one time loop
               PAVLC := PHANDLE ;

     //************************************************************
     // check handle and return if handle nok                      
     //************************************************************

               if PAVLC = NIL then
                 begin
                   RC := 20 ;
                   break
                 end (* then *) ;
               if PAVLC -> . MAGIC <> 'AVLCACHE' then
                 begin
                   RC := 20 ;
                   break
                 end (* then *) ;

     //************************************************************
     // output the whole cache using AVLGET                        
     //************************************************************

               WRITELN ;
               PC254 := PKEY ;
               if PC254 <> NIL then
                 WRITELN ( 'Print CACHE - Cachename = ' , SUBSTR (
                           PC254 -> , 1 , 8 ) )
               else
                 WRITELN ( 'Print CACHE - Cachename = *unknown*' ) ;
               WRITELN ( 'Number of entries = ' , PAVLC -> . COUNT : 1
                         ) ;
               WRITELN ( '----------------------------------------' ) ;
               RC := 8 ;
               SEQKEY_LOCAL := NIL ;
               PBAUMX := PAVLC -> . PTREE ;
               while TRUE do
                 begin
                   RCFOUND := AVLGET ( 'N' , PBAUMX , SEQKEY_LOCAL ,
                              PKEY_LOCAL , LKEY_LOCAL , PDAT_LOCAL ,
                              LDAT_LOCAL ) ;
                   if RCFOUND <> 0 then
                     break ;
                   WRITELN ;
                   PC254 := PKEY_LOCAL ;
                   WRITELN ( 'key: ' , SUBSTR ( PC254 -> , 1 ,
                             LKEY_LOCAL ) ) ;
                   PC254 := PDAT_LOCAL ;
                   WRITELN ( 'dat: ' , SUBSTR ( PC254 -> , 1 ,
                             LDAT_LOCAL ) ) ;
                   RC := 0 ;
                 end (* while *) ;
               WRITELN ;
             until TRUE
           end (* tag/ca *) ;
       7 : begin
             
           end (* tag/ca *) ;
       8 : begin
             
           end (* tag/ca *) ;
       9 : begin
             repeat  // START - one time loop
               PAVLC := PHANDLE ;

     //************************************************************
     // check handle and return if handle nok                      
     //************************************************************

               if PAVLC = NIL then
                 begin
                   RC := 20 ;
                   break
                 end (* then *) ;
               if PAVLC -> . MAGIC <> 'AVLCACHE' then
                 begin
                   RC := 20 ;
                   break
                 end (* then *) ;

     //************************************************************
     // search for key and return data if found                    
     //************************************************************

               PBAUMX := PAVLC -> . PTREE ;
               HCHANGED := FALSE ;
               PRES := AVLSRCHX ( PKEY , LKEY , PKEYNEU , LKEYNEU ,
                       GEFUNDEN , PBAUMX , HCHANGED , FALSE , '>' ,
                       CACHECOMP ) ;
               if not GEFUNDEN then
                 begin
                   PDAT := NIL ;
                   LDAT := 0 ;
                   RC := 8 ;
                 end (* then *)
               else
                 begin
                   PKEY := PKEYNEU -> ;
                   LKEY := LKEYNEU -> ;
                   PRES := AVLSRCHX ( PKEY , LKEY , POBJ , POBJLEN ,
                           GEFUNDEN , PBAUMX , HCHANGED , FALSE , '=' ,
                           CACHECOMP ) ;
                   PDAT := POBJ -> ;
                   LDAT := POBJLEN -> ;
                   RC := 0 ;
                 end (* else *)
             until TRUE
           end (* tag/ca *) ;
       10 : begin
              repeat // START - one time loop
                PAVLC := PHANDLE ;

     //************************************************************
     // check handle and return if handle nok                      
     //************************************************************

                if PAVLC = NIL then
                  begin
                    RC := 20 ;
                    break
                  end (* then *) ;
                if PAVLC -> . MAGIC <> 'AVLCACHE' then
                  begin
                    RC := 20 ;
                    break
                  end (* then *) ;

     //************************************************************
     // search for key and return data if found                    
     //************************************************************

                PBAUMX := PAVLC -> . PTREE ;
                AVLFREE ( PBAUMX ) ;
                PAVLC -> . PTREE := NIL ;
                PAVLC -> . COUNT := 0 ;
              until TRUE
            end (* tag/ca *) ;
       otherwise
         begin
           
         end (* otherw *)
     end (* case *) ;
     AVLCACHE := RC ;
   end (* AVLCACHE *) ;



begin (* HAUPTPROGRAMM *)
  
end (* HAUPTPROGRAMM *) .
