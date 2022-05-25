/*********************************************************************/
/*                                                                   */
/* System     : ADIP                                                 */
/*                                                                   */
/* Teilgebiet : X = Basisdienste                                     */
/*                                                                   */
/* Sourcefile : ADIPXHIL (Hilfsfunktionen)                           */
/*                                                                   */
/* Aufgabe    : diverse Hilfsfunktionen, Sortierung, Rundung         */
/*                                                                   */
/* Funktionen :                                                      */
/*                                                                   */
/* Autor(en)  : Engels / Oppolzer                                    */
/*                                                                   */
/*********************************************************************/
/*                                                                   */
/* Datum      ! Aenderer    ! was geaendert ?                        */
/* -----------!-------------!--------------------------------------  */
/* 25.03.2005 ! Engels      ! aus adipfint.c ausgelagert             */
/* .......... ! .......     ! .....................................  */
/* .......... ! .......     ! .....................................  */
/* .......... ! .......     ! .....................................  */
/* .......... ! .......     ! .....................................  */
/*                                                                   */
/*********************************************************************/


#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <math.h>
#include <time.h>



#include "adipt.h"
#include "adipx.h"



/*********************************************************************/
/*                                                                   */
/*   Funktion: adipx_trim                                            */
/*                                                                   */
/*  Separator vorne und hinten eliminieren                           */
/*                                                                   */
/*********************************************************************/

int adipx_trim (char *string, char separator)

{
   char *cp1;
   char *cp2;

   /***********************/
   /* Hinten abschnippeln */
   /***********************/

   for (cp1 = string + strlen (string) - 1;
        *cp1 == separator && cp1 >= string;
        *cp1 = '\0', cp1--)
      ;

   /***********************/
   /* Vorne abschnippeln  */
   /***********************/

   for (cp1 = string; *cp1 == separator; cp1++)
      ;

   /********************************************/
   /* strcpy usw. nicht nehmen wg. öberlappung */
   /********************************************/

   if (cp1 == string)
      return 0;

   for (cp2 = string; *cp1 != '\0'; cp1++, cp2++)
      *cp2 = *cp1;

   *cp2 = '\0';

   return 0;
}






/**********************************************************/
/*                                                        */
/*   adipx_sort                                           */
/*                                                        */
/*   Einfaches Sortierverfahren                           */
/*   mit quadratischem Aufwand                            */
/*                                                        */
/**********************************************************/

void adipx_sort (void *tab,
                 int size,
                 int left,
                 int right,
                 void *zusatz1,
                 void *zusatz2,
                 int (*comp) (void *, void *, void *, void *))

{
   int i;
   int j;
   char *ai;
   char *aj;
   char *am;
   char w [1024];

   for (ai = tab, i = left;
        i < right;
        ai += size, i ++)
   {
      am = ai;

      for (aj = ai + size, j = i + 1;
           j <= right;
           aj += size, j ++)
      {
         if ((*comp)(aj, am, zusatz1, zusatz2) < 0)
         {
            am = aj;
         }
      }

      if (am != ai)
      {
         memcpy (w, am, size);
         memcpy (am, ai, size);
         memcpy (ai, w, size);
      }
   }
}






/**********************************************************/
/*                                                        */
/*   adipx_qsort                                          */
/*                                                        */
/*   Quicksort lt. C.A.R. Hoare                           */
/*                                                        */
/*   w und x kînnen static sein, da sie trotz             */
/*   Rekursion nicht von verschiedenen Ebenen             */
/*   gleichzeitig gebraucht werden.                       */
/*                                                        */
/*   zusatz1 und zusatz2 sind Informationen, die          */
/*   an die Vergleichsfunktion durchgereicht werden,      */
/*   sie steuern den Vergleich                            */
/*                                                        */
/*   Die Maximalgrî·e eines Elements ist 2048,            */
/*   das Einhalten dieser Grî·e wird NICHT ÅberprÅft      */
/*                                                        */
/**********************************************************/

void adipx_qsort (void *tab,
                  int size,
                  int left,
                  int right,
                  void *zusatz1,
                  void *zusatz2,
                  int (*comp) (void *, void *, void *, void *))

{
   int i;
   int j;
   int m;

   char *atab = tab;
   char *ai;
   char *aj;

   static char w [2048];
   static char x [2048];

   TRACE_STRT ("adipx_qsort");
   TRACE_MESS ("atab    = %p\n", atab);
   TRACE_MESS ("size    = %d\n", size);
   TRACE_MESS ("left.a  = %d\n", left);
   TRACE_MESS ("right.a = %d\n", right);
   TRACE_MESS ("zusatz1 = <%s>\n", zusatz1);
   TRACE_MESS ("zusatz2 = <%s>\n", zusatz2);

   i = left;
   j = right;

   m = (left + right) / 2;
   memcpy (x, atab + m * size, size);

   ai = atab + i * size;
   aj = atab + j * size;

   do
   {
      while ((*comp)(ai, x, zusatz1, zusatz2) < 0)
         ai += size;

      while ((*comp)(x, aj, zusatz1, zusatz2) < 0)
         aj -= size;

      if (ai <= aj)
      {
         if (ai < aj)
         {
            memcpy (w, ai, size);
            memcpy (ai, aj, size);
            memcpy (aj, w, size);
         }

         ai += size;
         aj -= size;
      }
   }
   while (ai <= aj);

   i = (ai - atab) / size;
   j = (aj - atab) / size;

   if (left < j)
      adipx_qsort (atab, size, left, j, zusatz1, zusatz2, comp);

   if (i < right)
      adipx_qsort (atab, size, i, right, zusatz1, zusatz2, comp);

   TRACE_MESS ("left.e  = %d\n", left);
   TRACE_MESS ("right.e = %d\n", right);
   TRACE_STOP ("adipx_qsort");
}





/**********************************************************/
/*                                                        */
/*   adipx_bsearch                                        */
/*                                                        */
/*   BinÑre Suche                                         */
/*                                                        */
/**********************************************************/

void *adipx_bsearch (void *key,
                     void *tab,
                     int num,
                     int size,
                     int (*comp) (void *, void *, void *, void *))

{
   char *atab = tab;
   int a;
   int e;
   int m;
   int x;
   void *erg;

   TRACE_STRT ("adipx_bsearch");
   TRACE_MESS ("key     = %p\n", key);
   TRACE_MESS ("tab     = %p\n", tab);
   TRACE_MESS ("num     = %d\n", num);
   TRACE_MESS ("size    = %d\n", size);

   a = 0;
   e = num - 1;
   erg = NULL;

   while (a <= e)
   {
      m = (a + e) / 2;

      x = (*comp) (key, atab + m * size, NULL, NULL);
      if (x == 0)
      {
         erg = atab + m * size;
         break;
      }

      if (x < 0)
      {
         e = m - 1;
      }
      else
      {
         a = m + 1;
      }
   }

   TRACE_MESS ("m       = %d\n", m);
   TRACE_MESS ("erg     = %p\n", erg);
   TRACE_STOP ("adipx_bsearch");
   return erg;
}




double adipx_round (double wert, short bereich)

/**********************************************************/
/*                                                        */
/*   arPRound.c                                           */
/*                                                        */
/*   Rundungsfunktion neu mit geÑnderter Logik;           */
/*   die Korrekturkonstante wird anhand der Grî·en-       */
/*   ordnung des Ausgangswertes bestimmt (Ausgangs-       */
/*   wert durch (16 hoch 13); damit wird bei beiden       */
/*   Plattformen mindestens eine 1 an der letzten         */
/*   Ziffernposition dazuaddiert).                        */
/*                                                        */
/**********************************************************/

{
   static double fakttab [] =
        { 10000.0,
          1000.0,
          100.0,
          10.0,
          1.0,
          10.0,
          100.0,
          1000.0,
          10000.0,
          100000.0,
          1000000.0,
          10000000.0,
          100000000.0,
          1000000000.0,
          10000000000.0,
          100000000000.0,
          1000000000000.0,
          10000000000000.0,
          100000000000000.0,
          1000000000000000.0 } ;

   double faktor;
   double test;
   double rundkonst;

   faktor = fakttab [4 - bereich];

   if (wert < 0.0)
      test = -wert;
   else
      test = wert;

   if (test < 1.0e-55)
   {
      return 0.0;
   }

   /************************************************/
   /*                                              */
   /*   4 * (16 hoch 12)  =  1125899906842624.0    */
   /*   8 * (16 hoch 12)  =  2251799813685248.0    */
   /*  12 * (16 hoch 12)  =  3377699720527872.0    */
   /*        16 hoch 13   =  4503599627370496.0    */
   /*                                              */
   /************************************************/

#ifdef HOST
   rundkonst = test / 2251799813685248.0;
#else
   rundkonst = test / 4503599627370496.0;
#endif

   if (bereich < 0)
   {
      test = (test + rundkonst) * faktor + 0.5;
      test = floor (test);

      if (wert < 0.0)
      {
         return -test / faktor;
      }
      else
      {
         return test / faktor;
      }
   }
   else if (bereich > 0)
   {
      test = (test + rundkonst) / faktor + 0.5;
      test = floor (test);

      if (wert < 0.0)
      {
         return -test / faktor;
      }
      else
      {
         return test / faktor;
      }
   }
   else
   {
      test = (test + rundkonst) + 0.5;
      test = floor (test);

      if (wert < 0.0)
      {
         return -test;
      }
      else
      {
         return test;
      }
   }
}




int adipx_dverteil (double gesamt,
                    int anzahl,
                    double *pratio,
                    double *presult,
                    int nachk)

/***************************************************/
/*                                                 */
/*   Verteilung von double-Werten mit              */
/*   Korrektur der Auf- und Abrundung,             */
/*   so da· die Summation korrekt ist.             */
/*                                                 */
/*   gesamt:  Gesamtsumme                          */
/*   anzahl:  Anzahl der Elemente                  */
/*   pratio:  Steuerung der Verteilung             */
/*   presult: Ergebnis                             */
/*   nachk:   Nachkommastelle analog arPRound      */
/*                                                 */
/*   RÅckgabe des Returncodes wie folgt:           */
/*                                                 */
/*   0      - normale Verarbeitung                 */
/*   4      - gesamt ist Null                      */
/*   8      - Summe der pratio-Werte ist Null,     */
/*            Gesamt jedoch ungleich Null          */
/*            (presult ist in diesem Fall          */
/*            undefiniert)                         */
/*                                                 */
/***************************************************/

{
   double summea;
   double summeb;
   double h1;
   int i;
   int rc;
   double *pa, *pb;
   double genau;
   double *pmax, *pmin;
   double diff;
   double maxdiff, mindiff;

   static double genautab [] =
        { 1.0,
          0.1,
          0.01,
          0.001,
          0.0001,
          0.00001,
          0.000001,
          0.0000001,
          0.00000001,
          0.000000001,
          0.0000000001 } ;


   TRACE_STRT ("adipx_dverteil");

   rc = 0;

   /************************************************/
   /*   Runden des vorgegebenen Gesamtwerts        */
   /************************************************/

   gesamt = adipx_round (gesamt, nachk);

   if (gesamt == 0.0)
   {
      for (pa = presult, i = 1;
           i <= anzahl;
           pa++, i++)
         *pa = 0.0;

      rc = 4;
      TRACE_RETC (rc);
      TRACE_STOP ("adipx_dverteil");
      return rc;
   }

   /************************************************/
   /*   Ermitteln der Summe des pratio-Vektors     */
   /************************************************/

   for (summea = 0.0, pa = pratio, i = 1;
        i <= anzahl;
        pa++, i++)
      summea += *pa;

   if (adipx_round (summea, nachk) == 0.0)
   {
      rc = 8;
      TRACE_RETC (rc);
      TRACE_STOP ("adipx_dverteil");
      return rc;
   }

   /************************************************/
   /*   Berechnen der Bruchteile                   */
   /*   inklusive minimaler und maximaler          */
   /*   Differenz sowie Summe                      */
   /************************************************/

   for (summeb = 0.0, pa = presult, pb = pratio, i = 1;
        i <= anzahl;
        pa++, pb++, i++)
   {
      h1 = gesamt * (*pb / summea);
      *pa = adipx_round (h1, nachk);

      summeb += *pa;
      diff = h1 - *pa;
      if (i == 1)
      {
         maxdiff = diff;
         mindiff = diff;
         pmax = pa;
         pmin = pa;
      }
      else
      {
         if (diff < mindiff)
         {
            mindiff = diff;
            pmin = pa;
         }
         if (diff > maxdiff)
         {
            maxdiff = diff;
            pmax = pa;
         }
      }
   }

   genau = genautab [- nachk] * 0.1;

   /************************************************/
   /*   Bei Abweichung:                            */
   /************************************************/

   while ( fabs (gesamt - summeb) > genau )
   {
      /************************************************/
      /*   Anpassen der Elemente mit der grî·ten      */
      /*   bzw. kleinsten Differenz                   */
      /************************************************/

      if (summeb < gesamt)
         *pmax += genautab [- nachk];
      else if (summeb > gesamt)
         *pmin -= genautab [- nachk];

      /************************************************/
      /*   ... und neu berechnen                      */
      /************************************************/

      for (summeb = 0.0, pa = presult, pb = pratio, i = 1;
           i <= anzahl;
           pa++, pb++, i++)
      {
         h1 = gesamt * (*pb / summea);

         summeb += *pa;
         diff = h1 - *pa;
         if (i == 1)
         {
            maxdiff = diff;
            mindiff = diff;
            pmax = pa;
            pmin = pa;
         }
         else
         {
            if (diff < mindiff)
            {
               mindiff = diff;
               pmin = pa;
            }
            if (diff > maxdiff)
            {
               maxdiff = diff;
               pmax = pa;
            }
         }
      }
   }

   TRACE_RETC (rc);
   TRACE_STOP ("adipx_dverteil");
   return rc;
}





void adipx_string_to_upper (char *cp, int sl)

{
   int i;

   for (i = 0; i < sl; i++, cp ++)
      *cp = toupper (*cp);

   return;
}






size_t adipx_sysdate (char * dest, int laenge)

{
   /************************************************/
   /*   Ermittelt Tagesdatum und Uhrzeit in der    */
   /*   Form tt.mm.jjjj hh:mm:ss                   */
   /************************************************/

   time_t temp;
   struct tm *timeptr;
   size_t result;

   temp = time (NULL);
   timeptr = localtime (&temp);
   result = strftime (dest, laenge,
                      "%d.%m.%Y %H:%M:%S", timeptr);

   return result;
}






size_t adipx_systimest (char * dest, int laenge)

{
   /************************************************/
   /*   Ermittelt Tagesdatum und Uhrzeit in der    */
   /*   Form jjjjmmtthhmmss                        */
   /************************************************/

   time_t temp;
   struct tm *timeptr;
   size_t result;

   temp = time (NULL);
   timeptr = localtime (&temp);
   result = strftime (dest, laenge,
                      "%Y%m%d%H%M%S", timeptr);

   return result;
}





/**********************************************************/
/*                                                        */
/*   Funktion djulian                                     */
/*                                                        */
/**********************************************************/

static long djulian (int jahr, int lfdtag)

{
   long erg;

   /************************************************/
   /*   Konvertiert Datum der Form jahr/lfdtag     */
   /*   in julianisches Datum                      */
   /************************************************/

   if (jahr < 1583 || jahr > 2299)
      return 0;

   erg = 2299239 + 365 * (long) (jahr - 1583)
         + ((jahr - 1581) / 4);

   if (jahr >= 2201)
      erg -= 5;
   else if (jahr >= 2101)
      erg -= 4;
   else if (jahr >= 1901)
      erg -= 3;
   else if (jahr >= 1801)
      erg -= 2;
   else if (jahr >= 1701)
      erg -= 1;

   return (erg + lfdtag - 1);
}





/**********************************************************/
/*                                                        */
/*   Funktion jul2date                                    */
/*                                                        */
/**********************************************************/

static long jul2date (long djul)

{
   /************************************************/
   /*   Zu julianischem Datum (djul) wird die      */
   /*   Åbliche Datumsdarstellung inkl. lfd.       */
   /*   Tag im Jahr berechnet.                     */
   /************************************************/

   int schalt, lf, i;
   int jj, mm, tt;
   int lfdtag;
   int x;

   static int montab [] = { 31, 28, 31,
                            30, 31, 30,
                            31, 31, 30,
                            31, 30, 31 };

   if (djul < 2299239 || djul > 2561117)
      return 0;

   jj = (int) ((djul - 2299239) / 365) + 1583;
   while (jj > 2299 || djulian (jj, 1) > djul)
      jj --;

   lfdtag = (int) (djul - djulian (jj, 1)) + 1;

   schalt = (jj % 4 == 0 && (jj % 100 != 0 || jj % 400 == 0));

   i = 0;
   lf = lfdtag;
   while (lf > (x = montab [i] + (i == 1 ? schalt : 0)))
   {
      lf -= x;
      i ++;
   }

   mm = i + 1;
   tt = lf;

   return jj * 10000 + mm * 100 + tt;
}






/**********************************************************/
/*                                                        */
/*   Funktion add_tage                                    */
/*                                                        */
/**********************************************************/

long adipx_add_tage (long datum, long increment)

/************************************************/
/*                                              */
/*   Diese Funktion erhÑlt ein Datum in         */
/*   der Form jjjjmmtt und ein increment        */
/*   (Anzahl Tage, kann auch negativ sein).     */
/*   Das entsprechend modifizierte Datum        */
/*   wird zurÅckgegeben (datum wird             */
/*   modifiziert).                              */
/*                                              */
/*   Die Datumsangaben mÅssen im Bereich        */
/*   1.1.1583 bis 31.12.2299 liegen             */
/*                                              */
/************************************************/

{
   int jahr;
   int monat;
   int tag;
   long rest;
   int lfdtag;
   long jul;

   jahr = datum / 10000;
   rest = datum % 10000;
   monat = rest / 100;
   tag = rest % 100;

   lfdtag = adipx_dlfdtag (jahr, monat, tag);
   jul = djulian (jahr, lfdtag) + increment;

   return jul2date (jul);
}






/**********************************************************/
/*                                                        */
/*   Funktion diff_tage                                   */
/*                                                        */
/**********************************************************/

long adipx_diff_tage (long datum1, long datum2)

/************************************************/
/*                                              */
/*   ermittelt die Tage zwischen zwei           */
/*   Datumsangaben (0 = beide gleich, positiv   */
/*                  wenn datum1 > datum2)       */
/*                                              */
/*   Die Datumsangaben mÅssen im Bereich        */
/*   1.1.1583 bis 31.12.2299 liegen             */
/*                                              */
/************************************************/

{
   long rest;
   int jahr_gleich;

   int jahr1;
   int monat1;
   int tag1;
   int lfdtag1;
   long jul1;

   int jahr2;
   int monat2;
   int tag2;
   int lfdtag2;
   long jul2;

   jahr1 = datum1 / 10000;
   rest = datum1 % 10000;
   monat1 = rest / 100;
   tag1 = rest % 100;

   jahr2 = datum2 / 10000;
   rest = datum2 % 10000;
   monat2 = rest / 100;
   tag2 = rest % 100;

   jahr_gleich = (jahr1 == jahr2);

   if (jahr_gleich && monat1 == monat2)
   {
      return tag1 - tag2;
   }

   lfdtag1 = adipx_dlfdtag (jahr1, monat1, tag1);
   lfdtag2 = adipx_dlfdtag (jahr2, monat2, tag2);

   if (jahr_gleich)
   {
      return lfdtag1 - lfdtag2;
   }

   jul1 = djulian (jahr1, lfdtag1);
   jul2 = djulian (jahr2, lfdtag2);

   return jul1 - jul2;
}





/**********************************************************/
/*                                                        */
/*   Funktion dlfdtag                                     */
/*                                                        */
/**********************************************************/

int adipx_dlfdtag (int jj, int mm, int tt)

{
   /************************************************/
   /*   Berechnet zu tt.mm.jj den laufenden        */
   /*   Tag im Jahr                                */
   /************************************************/

   int schalt;
   int laufm;
   int lfd;

   static int montab [] = { 31, 28, 31, 30, 31, 30,
                            31, 31, 30, 31, 30, 31 };

   schalt = (jj % 4 == 0 &&
              (jj % 100 != 0 || jj % 400 == 0));

   lfd = 0;
   for (laufm = 0; laufm < mm-1; laufm++)
      lfd += montab [laufm]  + (laufm == 1 ? schalt : 0);
   lfd += tt;

   return lfd;

}





int adipx_ostern (int jahr, int *oster, int *schalt)

/************************************************/
/*                                              */
/*   Berechnung des Ostertags                   */
/*                                              */
/*   Gleichzeitig wird geprueft, ob es sich     */
/*   um ein Schaltjahr handelt.                 */
/*                                              */
/*   oster und schalt werden ermittelt und      */
/*   zurÅckgegeben.                             */
/*                                              */
/************************************************/

{
   int a, b, c, d, e;
   int eh, dh;

   if (jahr < 1583)
      return (-1);
   if (jahr > 2299)
      return (-1);

   if (jahr <= 1699)
      dh = 22;
   else if (jahr <= 1899)
      dh = 23;
   else if (jahr <= 2199)
      dh = 24;
   else
      dh = 25;

   if (jahr <= 1699)
      eh = 2;
   else if (jahr <= 1799)
      eh = 3;
   else if (jahr <= 1899)
      eh = 4;
   else if (jahr <= 2099)
      eh = 5;
   else if (jahr <= 2199)
      eh = 6;
   else
      eh = 0;

   a = jahr % 19;
   b = jahr % 4;
   c = jahr % 7;
   d = (19 * a + dh) % 30;
   e = (2 * b + 4 * c + 6 * d + eh) % 7;
   *oster = d + e;
   if (*oster == 35)
      *oster = 28;
   if (d == 28 && e == 6 && a > 10)
      *oster = 27;

   *schalt = (b == 0 && (jahr % 100 != 0 || jahr % 400 == 0));
   *oster += *schalt + 81;

   return(0);
}

