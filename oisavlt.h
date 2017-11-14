

#if 0





/********************************************************************/
/*                                                                  */
/*   Hier zunaechst ein kleines Hauptprogramm                       */
/*   zum Test dieser AVL-Baum-Funktionen                            */
/*                                                                  */
/*   zeigt ausserdem, wie sie verwendet werden.                     */
/*                                                                  */
/********************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>



#define BMNAME           TEST
#define BMKEYTYP         int
#define BMOBJTYP         int
#define OISAVLT_MALLOC   malloc
#define OISAVLT_FREE     free
#define BMKFUNC          compare_int
#define BMDRUCKFNX       druck_int

#define BM_DEFINITIONS
#define BM_CODE

#define BMDRUCKEN

#include "oisavlt.h"

#undef BMNAME
#undef BMKEYTYP
#undef BMOBJTYP
#undef OISAVLT_MALLOC
#undef OISAVLT_FREE
#undef BMKFUNC
#undef BMDRUCKFNX

#undef BM_DEFINITIONS
#undef BM_CODE

#undef BMDRUCKEN



static int compare_int (int *x, int *y)

{
   return *x - *y;
}


static void druck_int (FILE *f, int *x)

{
   fprintf (f, "%11d ", *x);
}





int main (int argc, char **argv)

{
   baumTESTknoten *proot = NULL;
   baumTESTknoten *pneu;
   int hchanged;

   int rc = 0;


   int i;
   for (i = 10; i <= 2000; i += 10)
   {
      hchanged = 0;
      pneu = baumTESTsuche (&i, &proot, &hchanged, 1);
      pneu -> obj = i * 10;

#if 0

      baumTESTdruck (proot, stdout, NULL, ' ');

      printf ("\n\n\n-----------------------------\n\n\n");

#endif

   }

   baumTESTdruck (proot, stdout, NULL, ' ');

   printf ("\n\n\n-----------------------------\n\n\n");

   rc = baumTESTget ('F', proot, &pneu);

   while (rc == 0)
   {
      printf ("gelesen: %d\n", pneu -> key);

      rc = baumTESTget ('N', pneu, &pneu);
   }

   printf ("\n\n\n-----------------------------\n\n\n");

   rc = baumTESTget ('L', proot, &pneu);

   while (rc == 0)
   {
      printf ("gelesen: %d\n", pneu -> key);

      rc = baumTESTget ('P', pneu, &pneu);
   }

   return 0;
}






#endif







/**********************************************************/
/*                                                        */
/*   Include-File fuer die Bearbeitung dynamischer        */
/*   ausgewogener Baeume - Oktober 2003                   */
/*                                                        */
/*   erstellt von Bernd Oppolzer                          */
/*                                                        */
/*   (siehe Wirth: Algorithmen und Datenstrukturen        */
/*   Teubner/Stuttgart 1975 - Seiten 296/297)             */
/*                                                        */
/*   Das Vorhandensein folgender Makrovariablen           */
/*   wird vorausgesetzt:                                  */
/*                                                        */
/*   BMNAME          - Name des Baumes (moeglichst kurz)  */
/*   BMKEYTYP        - Schluesseltyp (Struktur)           */
/*   BMOBJTYP        - Objekttyp (Struktur)               */
/*   OISAVLT_MALLOC  - alloc-Funktion                     */
/*   OISAVLT_FREE    - free-Funktion                      */
/*                                                        */
/*   optional weitere Makrovariablen:                     */
/*                                                        */
/*   BMDRUCKEN  - Funktion zum Drucken des Baums erz.     */
/*   BMEINRUECK - Anzahl Einrueckzeichen                  */
/*                                                        */
/*   Die folgenden Makrovariablen                         */
/*   werden aus BMNAME abgeleitet:                        */
/*                                                        */
/*   BMKNOTEN   - baum ## BMNAME ## knoten                */
/*   BMSUCHE    - baum ## BMNAME ## suche                 */
/*   BMGET      - baum ## BMNAME ## get                   */
/*   BMDRUCKFN  - baum ## BMNAME ## druck                 */
/*                                                        */
/*   Der Vergleich fuer die Schluessel wird ueber         */
/*   die Funktion BMKFUNC gemacht; diese Funktion         */
/*   ist geeignet zu implementieren.                      */
/*                                                        */
/*   BMKFUNC    - Vergleichsfunktion fuer Schluessel      */
/*                (zwei Schluesselstrukturpointer,        */
/*                Rueckgabe analog strcmp)                */
/*                                                        */
/**********************************************************/




#ifndef __OISAVLT_H__

#define __OISAVLT_H__

/************************************************/
/*   typedef fuer die Verbindungen              */
/************************************************/

typedef struct verb
{
   char zeichnen;
   struct verb *next;
   struct verb *prev;
}
baum_verbindung;

#endif





#define CONCAT(x,y)    x ## y
#define BMCONCAT(x,y)  CONCAT(x,y)

#define BMKNOTEN       BMCONCAT(BMCONCAT(baum,BMNAME),knoten)
#define BMSUCHE        BMCONCAT(BMCONCAT(baum,BMNAME),suche)
#define BMGET          BMCONCAT(BMCONCAT(baum,BMNAME),get)
#define BMFREE         BMCONCAT(BMCONCAT(baum,BMNAME),free)

#ifdef BMDRUCKEN

#define BMDRUCKFN      BMCONCAT(BMCONCAT(baum,BMNAME),druck)

#ifndef BMEINRUECK
#define BMEINRUECK     8
#endif

#endif





#ifdef BM_DEFINITIONS


typedef struct BMKNOTEN
{                                      /**************************/
   struct BMKNOTEN *pvorg;             /*  Vorgaenger            */
   struct BMKNOTEN *pln;               /*  linker Nachfolger     */
   struct BMKNOTEN *prn;               /*  rechter Nachfolger    */
   int balance;                        /*  balance lt. Wirth     */
   BMKEYTYP key;                       /*  Schluessel            */
   BMOBJTYP obj;                       /*  eigentliche Info      */
}                                      /**************************/
BMKNOTEN;


BM_STATIC BMKNOTEN *BMSUCHE (BMKEYTYP *skey,
                             BMKNOTEN **pp,
                             int *hchanged,
                             int einfuegen);

BM_STATIC int BMGET (char modus,
                     BMKNOTEN *start,
                     BMKNOTEN **result);

BM_STATIC void BMFREE (BMKNOTEN *p);


#ifdef BMDRUCKEN

BM_STATIC void BMDRUCKFN (BMKNOTEN *p,
                          FILE *ausg_file,
                          baum_verbindung *pv,
                          char richtung);

#endif


#endif






#ifdef BM_CODE






BM_STATIC BMKNOTEN *BMSUCHE (BMKEYTYP *skey,
                             BMKNOTEN **pp,
                             int *hchanged,
                             int einfuegen)

{
   BMKNOTEN *p1;
   BMKNOTEN *p2;
   BMKNOTEN *p;
   BMKNOTEN *px;

   BMKNOTEN *pres;

   BMKNOTEN *pvorg_save;

   p = *pp;


   if (p == NULL)
   {
      /************************************************************/
      /* Der Knoten existiert noch nicht, der gesuchte Schluessel */
      /* wurde also nicht gefunden.                               */
      /************************************************************/

      if (einfuegen != 0)
      {
         /************************************************************/
         /* Es wird ein neuer Knoten mit diesem Schluessel angelegt  */
         /* und in den Baum eingefuegt.                              */
         /************************************************************/

         p = OISAVLT_MALLOC (sizeof (BMKNOTEN));
         memset (p, 0x00, sizeof (BMKNOTEN));

         p -> key = *skey;
         *hchanged = 1;

         pres = p;
      }
      else
      {
         pres = NULL;
      }
   }
   else if (BMKFUNC (skey, &(p -> key)) < 0)
   {
      /************************************************************/
      /* Der gesuchte Schluessel ist kleiner als der Schluessel   */
      /* des aktuellen Knotens. Es wird also im linken Teilbaum   */
      /* weitergesucht (rekursiver Aufruf). Nachdem das passiert  */
      /* ist, wird geprueft, ob sich der linke Teilbaum durch ein */
      /* eventuelles Einfuegen verlaengert hat.                   */
      /************************************************************/

      pres = BMSUCHE (skey, &(p -> pln), hchanged, einfuegen);

      if (einfuegen != 0 && *hchanged == 1)
      {
         pvorg_save = p -> pvorg;

         /**************************************************/
         /* Falls der linke Teilbaum laenger geworden ist: */
         /**************************************************/

         switch (p -> balance)
         {
            case 1:

               /********************************************/
               /* bisher war der rechte Teilbaum laenger   */
               /********************************************/

               p -> balance = 0;
               *hchanged = 0;
               break;

            case 0:

               /*********************************************/
               /* bisher waren beide Teilbaeume gleich lang */
               /*********************************************/

               p -> balance = -1;
               break;

            case -1:

               /***************************************************/
               /* Der linke Teilbaum war ohnehin schon laenger.   */
               /* Jetzt muss der Baum umorganisiert werden!       */
               /* Zunaechst wird geprueft, ob beim linken Nach-   */
               /* folger der linke Teilbaum laenger ist (Fall A)  */
               /* oder der rechte (Fall B). Danach werden die     */
               /* Verbindungszeiger neu gesetzt.                  */
               /***************************************************/

               p1 = p -> pln;

               if (p1 -> balance == - 1)       /* Fall A */
               {
                  px = p1 -> prn;
                  p -> pln = px;
                  if (px != NULL)
                     px -> pvorg = p;

                  p1 -> prn = p;
                  p -> pvorg = p1;

                  p -> balance = 0;
                  p = p1;
               }
               else                            /* Fall B */
               {
                  p2 = p1 -> prn;

                  px = p2 -> pln;
                  p1 -> prn = px;
                  if (px != NULL)
                     px -> pvorg = p1;

                  p2 -> pln = p1;
                  p1 -> pvorg = p2;

                  px = p2 -> prn;
                  p -> pln = px;
                  if (px != NULL)
                     px -> pvorg = p;

                  p2 -> prn = p;
                  p -> pvorg = p2;

                  if (p2 -> balance == -1)
                     p -> balance = 1;
                  else
                     p -> balance = 0;

                  if (p2 -> balance == 1)
                     p1 -> balance = -1;
                  else
                     p1 -> balance = 0;

                  p = p2;
               }

               p -> balance = 0;
               *hchanged = 0;

               break;
         }

         if (p -> pln != NULL)
            p -> pln -> pvorg = p;

         p -> pvorg = pvorg_save;
      }
   }
   else if (BMKFUNC (skey, &(p -> key)) > 0)
   {
      /************************************************************/
      /* Der gesuchte Schluessel ist groesser als der Schluessel  */
      /* des aktuellen Knotens. Es wird also im rechten Teilbaum  */
      /* weitergesucht (rekursiver Aufruf). Nachdem das passiert  */
      /* ist, wird geprueft ob sich der rechte Teilbaum durch ein */
      /* eventuelles Einfuegen verlaengert hat.                   */
      /************************************************************/

      pres = BMSUCHE (skey, &(p -> prn), hchanged, einfuegen);

      if (einfuegen != 0 && *hchanged == 1)
      {
         pvorg_save = p -> pvorg;

         /***************************************************/
         /* Falls der rechte Teilbaum laenger geworden ist: */
         /***************************************************/

         switch (p -> balance)
         {
            case -1:

               /********************************************/
               /* bisher war der linke Teilbaum laenger    */
               /********************************************/

               p -> balance = 0;
               *hchanged = 0;
               break;

            case 0:

               /*********************************************/
               /* bisher waren beide Teilbaeume gleich lang */
               /*********************************************/

               p -> balance = 1;
               break;

            case 1:

               /***************************************************/
               /* Der rechte Teilbaum war ohnehin schon laenger.  */
               /* Jetzt muss der Baum umorganisiert werden!       */
               /* Zunaechst wird geprueft, ob beim rechten Nach-  */
               /* folger der rechte Teilbaum laenger ist (Fall A) */
               /* oder der linke (Fall B). Danach werden die      */
               /* Verbindungszeiger neu gesetzt.                  */
               /***************************************************/

               p1 = p -> prn;

               if (p1 -> balance == 1)         /* Fall A */
               {
                  px = p1 -> pln;
                  p -> prn = px;
                  if (px != NULL)
                     px -> pvorg = p;

                  p1 -> pln = p;
                  p -> pvorg = p1;

                  p -> balance = 0;
                  p = p1;
               }
               else                            /* Fall B */
               {
                  p2 = p1 -> pln;

                  px = p2 -> prn;
                  p1 -> pln = px;
                  if (px != NULL)
                     px -> pvorg = p1;

                  p2 -> prn = p1;
                  p1 -> pvorg = p2;

                  px = p2 -> pln;
                  p -> prn = px;
                  if (px != NULL)
                     px -> pvorg = p;

                  p2 -> pln = p;
                  p -> pvorg = p2;

                  if (p2 -> balance == 1)
                     p -> balance = -1;
                  else
                     p -> balance = 0;

                  if (p2 -> balance == -1)
                     p1 -> balance = 1;
                  else
                     p1 -> balance = 0;

                  p = p2;
               }

               p -> balance = 0;
               *hchanged = 0;

               break;
         }

         if (p -> prn != NULL)
            p -> prn -> pvorg = p;

         p -> pvorg = pvorg_save;
      }
   }
   else
   {
      /***********************************************************/
      /* Schluessel gefunden, diesen Knoten zurueckgeben         */
      /***********************************************************/

      pres = p;

      if (einfuegen)
         *hchanged = 0;
   }

   *pp = p;

   return pres;
}






/********************************************************************/
/*                                                                  */
/*   suche nachfolger im baum:                                      */
/*                                                                  */
/*   vorwaerts:                                                     */
/*                                                                  */
/*   wenn rechter nachfolger da,                                    */
/*      gehe dorthin, und                                           */
/*      dann immer linker nachfolger, bis keiner mehr da ist.       */
/*      fertig                                                      */
/*                                                                  */
/*   wenn kein rechter nachfolger da:                               */
/*      wenn kein vorgaenger da: ende der iteration                 */
/*      wenn vorgaenger da:                                         */
/*         ist aktueller knoten linker nachfolger des vorgaengers?  */
/*         ja: vorgaenger nehmen und fertig.                        */
/*         nein: weiter mit vorgaenger                              */
/*                                                                  */
/*   rueckwaerts:                                                   */
/*                                                                  */
/*   wenn linker nachfolger da,                                     */
/*      gehe dorthin, und                                           */
/*      dann immer rechter nachfolger, bis keiner mehr da ist.      */
/*      fertig                                                      */
/*                                                                  */
/*   wenn kein linker nachfolger da:                                */
/*      wenn kein vorgaenger da: ende der iteration                 */
/*      wenn vorgaenger da:                                         */
/*         ist aktueller knoten rechter nachfolger des vorgaengers? */
/*         ja: vorgaenger nehmen und fertig.                        */
/*         nein: weiter mit vorgaenger                              */
/*                                                                  */
/*   erster und letzter ist einfach:                                */
/*                                                                  */
/*   nach oben und dann ganz links oder ganz rechts runter          */
/*                                                                  */
/*------------------------------------------------------------------*/
/*                                                                  */
/*   Die folgende Funktion navigiert auf dem Baum;                  */
/*   dabei kann wie ueblich first, last, next, previous             */
/*   mitgegeben werden (F,L,N,P).                                   */
/*                                                                  */
/*   Fuer start muss irgendein Knoten des Baums mitgegeben          */
/*   werden (bei F und L geht es am schnellsten mit dem             */
/*   root-Knoten, aber alle anderen gehen auch). Bei N und P        */
/*   wird der jeweils naechste bzw. vorhergehende zurueck-          */
/*   gegeben, ansonsten der erste bzw. letzte ueberhaupt.           */
/*                                                                  */
/*------------------------------------------------------------------*/
/*                                                                  */
/*   erstellt 12.2009 - OPP                                         */
/*                                                                  */
/********************************************************************/


BM_STATIC int BMGET (char modus,
                     BMKNOTEN *start,
                     BMKNOTEN **result)

{
   BMKNOTEN *res = NULL;
   BMKNOTEN *p;
   BMKNOTEN *p2;
   int rc = 0;

   if (start == NULL)
      rc = 2;

   switch (modus)
   {
      case 'F':
         if (rc != 0)
            break;

         while ((p = start -> pvorg) != NULL)
            start = p;

         while ((p = start -> pln) != NULL)
            start = p;

         res = start;
         break;

      case 'L':
         if (rc != 0)
            break;

         while ((p = start -> pvorg) != NULL)
            start = p;

         while ((p = start -> prn) != NULL)
            start = p;

         res = start;
         break;

      case 'N':
         if (rc != 0)
            break;

         if ((p = start -> prn) != NULL)
         {
            while ((p2 = p -> pln) != NULL)
               p = p2;
            res = p;
         }
         else
         {
            p = start;
            for (;;)
            {
               if (p -> pvorg == NULL)
               {
                  rc = 1;
                  break;
               }

               if (p == p -> pvorg -> pln)
               {
                  res = p -> pvorg;
                  break;
               }

               p = p -> pvorg;
            }
         }
         break;

      case 'P':
         if (rc != 0)
            break;

         if ((p = start -> pln) != NULL)
         {
            while ((p2 = p -> prn) != NULL)
               p = p2;
            res = p;
         }
         else
         {
            p = start;
            for (;;)
            {
               if (p -> pvorg == NULL)
               {
                  rc = 1;
                  break;
               }

               if (p == p -> pvorg -> prn)
               {
                  res = p -> pvorg;
                  break;
               }

               p = p -> pvorg;
            }
         }
         break;

      default:
         rc = -1;
         break;
   }

   *result = res;
   return rc;
}





BM_STATIC void BMFREE (BMKNOTEN *p)

{
   if (p != NULL)
   {
      BMFREE (p -> pln);
      BMFREE (p -> prn);
      OISAVLT_FREE (p);
   }
}





#ifdef BMDRUCKEN


BM_STATIC void BMDRUCKFN (BMKNOTEN *p,
                          FILE *ausg_file,
                          baum_verbindung *pv,
                          char richtung)

{
   int i;

   baum_verbindung vn;
   baum_verbindung *pvl;

   static const char sw = '-';
   static const char ss = '|';
   static baum_verbindung *pvfirst = NULL;

   if (p != NULL)
   {
      /*********************************************/
      /*   Rechten Teilbaum ausgeben. Dazu         */
      /*   Verbindungsstruktur an die              */
      /*   verkettete Liste dranhaengen            */
      /*********************************************/

      if (pv != NULL)
         pv -> next = &vn;
      else
         pvfirst = &vn;

      vn.prev = pv;
      vn.zeichnen = 'N';
      vn.next = NULL;

      BMDRUCKFN (p -> prn, ausg_file, &vn, 'R');

      /*********************************************************/
      /*   Schreiben der Key-Information und evtl. not-        */
      /*   wendiger Verbindungszeichen                         */
      /*********************************************************/

      if (pv != NULL)
      {
         fprintf (ausg_file, "%*c", BMEINRUECK, ' ');

         for (pvl = pvfirst; pvl != pv; pvl = pvl -> next)
         {
            fprintf (ausg_file, "%*c", BMEINRUECK,
                                (pvl -> zeichnen == 'J' ? ss : ' '));
         }
      }

#if 0

      if (p -> pvorg != NULL)
         printf ("%05d/", p -> pvorg -> key);
      else
         printf (" NULL/");

#endif

      BMDRUCKFNX (ausg_file, &(p -> key));

      if (pv != NULL)
      {
         if (richtung == 'R')
            pv -> zeichnen = 'J';
         else
            pv -> zeichnen = 'N';
      }

      if (p -> pln != NULL)
         vn.zeichnen = 'J';
      else
         vn.zeichnen = 'N';

      if (p -> prn != NULL || p -> pln != NULL)
      {
         /********************************************************/
         /*   Falls es Nachfolge-Knoten gibt, horizontale        */
         /*   Verbindungszeichen ausgeben                        */
         /********************************************************/

         for (i = 1; i <= BMEINRUECK - 5; i++)
            fputc (sw, ausg_file);

         fputc (ss, ausg_file);
      }

      fprintf (ausg_file, "\n");

      /*******************************************************/
      /*   Verbindungszeichen ausgeben                       */
      /*******************************************************/

      vn.next = NULL;

      fprintf (ausg_file, "%*c", BMEINRUECK, ' ');

      for (pvl = pvfirst; pvl != NULL; pvl = pvl -> next)
      {
         fprintf (ausg_file, "%*c", BMEINRUECK,
                             (pvl -> zeichnen == 'J' ? ss : ' '));
      }

      fprintf (ausg_file, "\n");

      /*********************************/
      /*   Linken Teilbaum ausgeben    */
      /*********************************/

      BMDRUCKFN (p -> pln, ausg_file, &vn, 'L');
   }

   return;
}


#endif






#endif





#undef CONCAT
#undef BMCONCAT
#undef BMKNOTEN
#undef BMSUCHE
#undef BMGET
#undef BMFREE
#undef BMDRUCKEN
#undef BMDRUCKFN
#undef BMEINRUECK


