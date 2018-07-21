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
                 PVORG : PTR_AVLNODE ;    // Vorgaenger
                 PLN : PTR_AVLNODE ;      // linker Nachfolger
                 PRN : PTR_AVLNODE ;      // rechter Nachfolger
                 BALANCE : INTEGER ;      // balance lt. Wirth
                 KEY : VOIDPTR ;          // Schluessel
                 KEYLEN : INTEGER ;       // Keylen if deep copy
                 OBJ : VOIDPTR ;          // eigentliche Info
                 OBJLEN : INTEGER ;       // Objlen if deep copy
               end ;
     PTR_AVLLINK = -> AVLLINK ;
     AVLLINK = record                     // only used for avlprint
                 ZEICHNEN : CHAR ;        // storage for character
                 NEXT : PTR_AVLLINK ;     // to print the tree
                 PREV : PTR_AVLLINK ;     // linkage lines
               end ;



function AVLSRCH ( SKEY : VOIDPTR ; SKEYLEN : INTEGER ; function
                 AVLCOMP ( X1 , X2 : VOIDPTR ) : INTEGER ; var PP :
                 PTR_AVLNODE ; var HCHANGED : BOOLEAN ; EINFUEGEN :
                 BOOLEAN ) : PTR_AVLNODE ;

(********************************************************************)
(*                                                                  *)
(*   Suchfunktion im AVL-Baum (fuegt auch ein, falls noetig)        *)
(*                                                                  *)
(*   skey = Zeiger auf Suchargument fuer Suche und ggf. Einfg.      *)
(*   skeylen = deep copy, wenn skeylen > 0 - dann wird der Key      *)
(*             beim Einfuegen in den AVL-Baum kopiert (ALLOC)       *)
(*             ansonsten wird nur der Pointer gespeichert           *)
(*   avlcomp = Vergleichsfunktion fuer Suchargumente, muss          *)
(*             Werte zurueckgeben wie memcmp (1, -1, 0)             *)
(*   pp = aktueller AVL-Baum, wird veraendert zurueckgegeben        *)
(*   hchanged = vorher immer auf false zu initialisieren            *)
(*   einfuegen = auf true setzen, wenn Key eingefuegt werden soll   *)
(*                                                                  *)
(*   Behandlung von skey beim Einfuegen siehe oben; haengt von      *)
(*   skeylen ab. Mit dem Pointer obj wird derzeit ueberhaupt        *)
(*   nichts gemacht (kommt evtl. spaeter).                          *)
(*                                                                  *)
(*   Nach AVLSRCH steht ueber das Funktionsergebnis der             *)
(*   Zeiger auf das zuletzt eingefuegte Baumelement zur             *)
(*   Verfuegung (dann koennte z.B. an obj ein Wert angehaengt       *)
(*   werden). Dazu muss aber beim Rufer die Struktur AVLNODE        *)
(*   bekannt sein (Rueckgabe nicht nur VOIDPTR).                    *)
(*                                                                  *)
(********************************************************************)


   var P1 : PTR_AVLNODE ;
       P2 : PTR_AVLNODE ;
       P : PTR_AVLNODE ;
       PX : PTR_AVLNODE ;
       PRES : PTR_AVLNODE ;
       PVORG_SAVE : PTR_AVLNODE ;

   begin (* AVLSRCH *)
     P := PP ;
     if P = NIL then
       begin

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
           end (* then *)
         else
           begin
             PRES := NIL ;
           end (* else *)
       end (* then *)
     else
       if AVLCOMP ( SKEY , P -> . KEY ) < 0 then
         begin

     /************************************************************/
     /* Der gesuchte Schluessel ist kleiner als der Schluessel   */
     /* des aktuellen Knotens. Es wird also im linken Teilbaum   */
     /* weitergesucht (rekursiver Aufruf). Nachdem das passiert  */
     /* ist, wird geprueft, ob sich der linke Teilbaum durch ein */
     /* eventuelles Einfuegen verlaengert hat.                   */
     /************************************************************/

           PRES := AVLSRCH ( SKEY , SKEYLEN , AVLCOMP , P -> . PLN ,
                   HCHANGED , EINFUEGEN ) ;
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
         if AVLCOMP ( SKEY , P -> . KEY ) > 0 then
           begin

     /************************************************************/
     /* Der gesuchte Schluessel ist groesser als der Schluessel  */
     /* des aktuellen Knotens. Es wird also im rechten Teilbaum  */
     /* weitergesucht (rekursiver Aufruf). Nachdem das passiert  */
     /* ist, wird geprueft ob sich der rechte Teilbaum durch ein */
     /* eventuelles Einfuegen verlaengert hat.                   */
     /************************************************************/

             PRES := AVLSRCH ( SKEY , SKEYLEN , AVLCOMP , P -> . PRN ,
                     HCHANGED , EINFUEGEN ) ;
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

     /***********************************************************/
     /* Schluessel gefunden, diesen Knoten zurueckgeben         */
     /***********************************************************/

             PRES := P ;
             if EINFUEGEN then
               HCHANGED := FALSE ;
           end (* else *) ;
     PP := P ;
     AVLSRCH := PRES ;
   end (* AVLSRCH *) ;



function AVLGET ( MODUS : CHAR ; START : PTR_AVLNODE ; var RESULT :
                PTR_AVLNODE ; var PKEY : VOIDPTR ; var POBJ : VOIDPTR )
                : INTEGER ;

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
       P2 : PTR_AVLNODE ;
       RC : INTEGER ;

   begin (* AVLGET *)
     RES := NIL ;
     PKEY := NIL ;
     POBJ := NIL ;
     RC := 0 ;
     if START = NIL then
       RC := 2 ;
     case MODUS of
       'F' : if RC = 0 then
               begin
                 while START -> . PVORG <> NIL do
                   START := START -> . PVORG ;
                 while START -> . PLN <> NIL do
                   START := START -> . PLN ;
                 RES := START ;
               end (* then *) ;
       'L' : if RC = 0 then
               begin
                 while START -> . PVORG <> NIL do
                   START := START -> . PVORG ;
                 while START -> . PRN <> NIL do
                   START := START -> . PRN ;
                 RES := START ;
               end (* then *) ;
       'N' : if RC = 0 then
               begin
                 P := START -> . PRN ;
                 if P <> NIL then
                   begin
                     while P -> . PLN <> NIL do
                       P := P -> . PLN ;
                     RES := P ;
                   end (* then *)
                 else
                   begin
                     P := START ;
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
                 P := START -> . PLN ;
                 if P <> NIL then
                   begin
                     while P -> . PRN <> NIL do
                       P := P -> . PRN ;
                     RES := P ;
                   end (* then *)
                 else
                   begin
                     P := START ;
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
         POBJ := RES -> . OBJ ;
       end (* then *) ;
     AVLGET := RC ;
   end (* AVLGET *) ;



procedure AVLPRINT ( P : PTR_AVLNODE ; var AUSGFILE : TEXT ; EINRUECK :
                   INTEGER ; procedure AVLPKEY ( var F : TEXT ; P :
                   VOIDPTR ) ; PV : PTR_AVLLINK ; RICHTUNG : CHAR ) ;

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
         AVLPRINT ( P -> . PRN , AUSGFILE , EINRUECK , AVLPKEY , ADDR (
                    VN ) , 'R' ) ;

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

         AVLPRINT ( P -> . PLN , AUSGFILE , EINRUECK , AVLPKEY , ADDR (
                    VN ) , 'L' ) ;
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



begin (* HAUPTPROGRAMM *)
  
end (* HAUPTPROGRAMM *) .
