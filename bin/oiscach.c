/**********************************************************/
/*                                                        */
/*   OISCACH.C                                            */
/*                                                        */
/*   Cache implementieren durch AVL-Baeume                */
/*                                                        */
/*   Autor: Bernd Oppolzer                                */
/*          2014                                          */
/*                                                        */
/**********************************************************/
/*   Historie:                                            */
/**********************************************************/
/*   13.12.2014 - OPP                                     */
/*   Fehler bei PUT:                                      */
/*   SPeicherleck bei schon vorhandenem Key               */
/**********************************************************/


#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>




/************************************************/
/*   SET: vorformatieren mit Hex-Nullen         */
/*   uebertragen in Laenge von y, maximal       */
/*   in Laenge von x minus eins                 */
/************************************************/

#define SET(x,y)   memset (x, 0x00, sizeof (x)),                  \
                   memcpy (x, y, (strlen (y) < sizeof (x) - 1 ?   \
                                  strlen (y) : sizeof (x) - 1))

/************************************************/
/*   SETB: vorformatieren mit Hex-Nullen        */
/*   uebertragen maximal bis zum ersten Blank   */
/*   in y, sonst Laenge von x minus eins        */
/************************************************/

#define SETB(x,y)  memset (x, 0x00, sizeof (x)),                  \
                   memcpy (x, y, (strcspn (y, " ") <              \
                                  sizeof (x) - 1 ?                \
                                  strcspn (y, " ") :              \
                                  sizeof (x) - 1))

/************************************************/
/*   SET: vorformatieren mit Blanks             */
/*   letztes Zeichen (Laenge l) = Hex Null      */
/*   uebertragen entsprechend strlen von y      */
/************************************************/

#define SETL(x,y,l)   memset (x, ' ', sizeof (x)),                   \
                      memcpy (x, y, (strlen (y) < sizeof (x) - 1 ?   \
                                     strlen (y) : sizeof (x) - 1)),  \
                      x [l] = 0x00



#ifdef ABSHOST
#define CLEAN_PTR(x)    ((void *)((unsigned int)(x) & 0x7fffffff))
#else
#define CLEAN_PTR(x)    (x)
#endif





typedef struct
{
   char name [9];
}
cachename;


typedef struct
{
   void *pbaum;
}
cacheptr;


static int verglcache (cachename *x1, cachename *x2)

{
   return strcmp (x1 -> name, x2 -> name);
}



#define BM_DEFINITIONS
#define BM_CODE
#define BM_STATIC        static
#define OISAVLT_MALLOC   malloc
#define OISAVLT_FREE     free

#define BMNAME      CACHEDIR
#define BMKEYTYP    cachename
#define BMOBJTYP    cacheptr
#define BMKFUNC     verglcache

#include "oisavlt.h"

#undef BMNAME
#undef BMKEYTYP
#undef BMOBJTYP
#undef BMKFUNC






typedef struct
{
   int laenge;
   char *daten;
}
cachekey;


typedef struct
{
   int laenge;
   char *daten;
}
cachedaten;


static int verglckey (cachekey *x1, cachekey *x2)

{
   int x;

   x = x1 -> laenge - x2 -> laenge;
   if (x != 0)
      return x;
   else
      return memcmp (x1 -> daten,
                     x2 -> daten,
                     x1 -> laenge);
}



#define BM_DEFINITIONS
#define BM_CODE
#define BM_STATIC        static
#define OISAVLT_MALLOC   malloc
#define OISAVLT_FREE     free

#define BMNAME      CACHE
#define BMKEYTYP    cachekey
#define BMOBJTYP    cachedaten
#define BMKFUNC     verglckey

#include "oisavlt.h"

#undef BMNAME
#undef BMKEYTYP
#undef BMOBJTYP
#undef BMKFUNC




int oiscach (char *funkcode,   // Funktionscode
             void *phandle,    // Zeiger auf Cachehandle
             void **seqkey,    // Zeiger fuer seq. Keyposition,
             char **pkey,      // Zeiger auf Zeiger auf Key
             int  *lkey,       // Zeiger auf Laenge Key
             char **pdat,      // Zeiger auf Zeiger auf Daten
             int  *ldat)       // Zeiger auf Laenge Daten

/**********************************************************/
/*                                                        */
/*   Funktion oiscach                                     */
/*                                                        */
/**********************************************************/

{
   int rc = 0;

   int hchanged;

   cachename cachek;
   static baumCACHEDIRknoten *pbaum = NULL;
   baumCACHEDIRknoten *pcached;

   cachekey xkey;
   baumCACHEknoten *pbaumx = NULL;
   baumCACHEknoten *pxdat;
   baumCACHEknoten *pneu;
   baumCACHEknoten *palt;

   char ausg_name [30];

   funkcode = CLEAN_PTR (funkcode);
   phandle = CLEAN_PTR (phandle);
   pkey = CLEAN_PTR (pkey);
   pdat = CLEAN_PTR (pdat);
   ldat = CLEAN_PTR (ldat);

   if (memcmp (funkcode, "CREATE", 6) == 0)
   {
      memset (&cachek, 0x00, sizeof (cachename));
      SET (cachek.name, *pkey);

#if 0
      printf ("CREATE <%s>\n", cachek.name);
#endif

      hchanged = 0;
      pcached = baumCACHEDIRsuche (&cachek, &pbaum, &hchanged, 1);

#if 0
      printf ("pcached = %p\n", pcached);
#endif

      if (pcached == NULL)
      {
         rc = 12;
         *pdat = NULL;
         *ldat = 0;
      }
      else
      {
         pcached -> obj.pbaum = NULL;
         *pdat = (char *) pcached;
         *ldat = 4;
         rc = 0;
      }

#if 0
      printf ("ptr aus baumsuche = %p\n", *pdat);
      printf ("rc CREATE = %d\n", rc);
#endif

   }
   else if (memcmp (funkcode, "GET", 3) == 0)
   {
      pcached = phandle;
      pbaumx = pcached -> obj.pbaum;

#if 0
      printf ("GET fuer Baum = %p\n", pcached);
      printf ("Rootknoten = %p\n", pbaumx);
#endif

      xkey.daten = *pkey;
      xkey.laenge = *lkey;

      hchanged = 0;
      pxdat = baumCACHEsuche (&xkey, &pbaumx, &hchanged, 0);

      if (pxdat == NULL)
      {
         rc = 8;
         *pdat = NULL;
         *ldat = 0;
      }
      else
      {
         *pdat = pxdat -> obj.daten;
         *ldat = pxdat -> obj.laenge;
         rc = 0;
      }

#if 0
      printf ("ptr aus baumsuche = %p\n", *pdat);
      printf ("len aus baumsuche = %d\n", *ldat);
      printf ("rc GET = %d\n", rc);
#endif

   }
   else if (memcmp (funkcode, "PUT", 3) == 0)
   {
      pcached = phandle;
      pbaumx = pcached -> obj.pbaum;

#if 0
      printf ("PUT fuer Baum = %p\n", pcached);
      printf ("Rootknoten = %p\n", pbaumx);
#endif

      xkey.daten = *pkey;
      xkey.laenge = *lkey;

      hchanged = 0;
      pxdat = baumCACHEsuche (&xkey, &pbaumx, &hchanged, 0);

      if (pxdat == NULL)
      {
         xkey.daten = malloc (*lkey);
         memcpy (xkey.daten, *pkey, *lkey);
         xkey.laenge = *lkey;

         hchanged = 0;
         pxdat = baumCACHEsuche (&xkey, &pbaumx, &hchanged, 1);

         pxdat -> obj.daten = malloc (*ldat);
         memcpy (pxdat -> obj.daten, *pdat, *ldat);
         pxdat -> obj.laenge = *ldat;

         pcached -> obj.pbaum = pbaumx;

#if 0
         printf ("Knoten neu eingefuegt\n");
#endif

         rc = 0;
      }
      else
      {
         if (pxdat -> obj.daten != NULL)
            free (pxdat -> obj.daten);

         pxdat -> obj.daten = malloc (*ldat);
         memcpy (pxdat -> obj.daten, *pdat, *ldat);
         pxdat -> obj.laenge = *ldat;

#if 0
         printf ("Knoten ersetzt\n");
#endif

         rc = 0;
      }

#if 0
      printf ("Rootknoten neu = %p\n", pbaumx);
      printf ("ptr aus baumsuche = %p\n", *pdat);
      printf ("len aus baumsuche = %d\n", *ldat);
      printf ("rc PUT = %d\n", rc);
#endif

   }
   else if (memcmp (funkcode, "GFIRST", 6) == 0)
   {
      pcached = phandle;
      pbaumx = pcached -> obj.pbaum;

#if 0
      printf ("GFIRST fuer Baum = %p\n", pcached);
      printf ("Rootknoten = %p\n", pbaumx);
#endif

      rc = baumCACHEget ('F', pbaumx, &pneu);

      if (rc != 0)
      {
         rc = 8;
         *pkey = NULL;
         *lkey = 0;
         *pdat = NULL;
         *ldat = 0;
      }
      else
      {
         *seqkey = pneu;
         *pkey = pneu -> key.daten;
         *lkey = pneu -> key.laenge;
         *pdat = pneu -> obj.daten;
         *ldat = pneu -> obj.laenge;
      }

#if 0
      printf ("keyptr aus baumsuche = %p\n", *pkey);
      printf ("keylen aus baumsuche = %d\n", *lkey);
      printf ("datptr aus baumsuche = %p\n", *pdat);
      printf ("datlen aus baumsuche = %d\n", *ldat);
      printf ("rc GFIRST = %d\n", rc);
      printf ("ptr kopieren wg. Leseposition = %p\n", pneu);
#endif

   }
   else if (memcmp (funkcode, "GNEXT", 5) == 0)
   {
      pcached = phandle;
      pbaumx = pcached -> obj.pbaum;

#if 0
      printf ("GNEXT fuer Baum = %p\n", pcached);
      printf ("Rootknoten = %p\n", pbaumx);
#endif

      palt = *seqkey;

#if 0
      printf ("ptr zum Start bei GNEXT = %p\n", palt);
#endif

      rc = baumCACHEget ('N', palt, &pneu);

      if (rc != 0)
      {
         rc = 8;
         *pkey = NULL;
         *lkey = 0;
         *pdat = NULL;
         *ldat = 0;
      }
      else
      {
         *seqkey = pneu;
         *pkey = pneu -> key.daten;
         *lkey = pneu -> key.laenge;
         *pdat = pneu -> obj.daten;
         *ldat = pneu -> obj.laenge;
      }

#if 0
      printf ("keyptr aus baumsuche = %p\n", *pkey);
      printf ("keylen aus baumsuche = %d\n", *lkey);
      printf ("datptr aus baumsuche = %p\n", *pdat);
      printf ("datlen aus baumsuche = %d\n", *ldat);
      printf ("rc GFIRST = %d\n", rc);
      printf ("ptr kopieren wg. Leseposition = %p\n", pneu);
#endif

   }
   else if (memcmp (funkcode, "TRACE", 5) == 0)
   {
      pcached = phandle;
      pbaumx = pcached -> obj.pbaum;

#if 0
      printf ("TRACE fuer Baum = %p\n", pcached);
      printf ("Rootknoten = %p\n", pbaumx);
#endif

      memcpy (ausg_name, *pkey, 8);
      ausg_name [8] = 0x00;

      printf ("\n\n\n"
              "CACHE ausdrucken - Cachename = %s\n"
              "-----------------------------\n\n\n",
              ausg_name);

      rc = baumCACHEget ('F', pbaumx, &pneu);

      while (rc == 0)
      {
         printf ("key: %-*.*s\n",
                  pneu -> key.laenge,
                  pneu -> key.laenge,
                  pneu -> key.daten);

         printf ("dat: %-*.*s\n",
                  pneu -> obj.laenge,
                  pneu -> obj.laenge,
                  pneu -> obj.daten);

         rc = baumCACHEget ('N', pneu, &pneu);
      }

      printf ("\n\n\n-----------------------------\n\n\n");

      rc = 0;
   }
   else if (memcmp (funkcode, "DELETE", 6) == 0)
   {
      pcached = phandle;
      pbaumx = pcached -> obj.pbaum;

      rc = baumCACHEget ('F', pbaumx, &pneu);

      while (rc == 0)
      {
         if (pneu -> key.daten != NULL)
            free (pneu -> key.daten);

         if (pneu -> obj.daten != NULL)
            free (pneu -> obj.daten);

         palt = pneu;
         rc = baumCACHEget ('N', palt, &pneu);
      }

#if 0
      printf ("DELETE auf Baum = %p\n", pcached);
      printf ("Rootknoten = %p\n", pbaumx);
#endif

      baumCACHEfree (pbaumx);

      pcached -> obj.pbaum = NULL;

      rc = 0;
   }
   else
   {
      rc = 20;
   }

   return rc;
}

