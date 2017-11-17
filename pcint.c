/**********************************************************************/
/*                                                                    */
/*  P-Code Interpreter                                                */
/*                                                                    */
/*  first to do:                                                      */
/*                                                                    */
/*  portable P-Code Assembler                                         */
/*                                                                    */
/*  Oppolzer / from 2012 until today                                  */
/*                                                                    */
/**********************************************************************/
/*                                                                    */
/*  To do:                                                            */
/*                                                                    */
/*  - zusaetzliche Operanden aus Folgezeilen fehlen in Listing        */
/*  - d.h. neue Struktur und an Befehle anketten, dann ausgeben       */
/*                                                                    */
/**********************************************************************/
/*                                                                    */
/*  Variante 01:                                                      */
/*                                                                    */
/*  Datum      ! Aenderer    ! was geaendert ?                        */
/*  -----------!-------------!--------------------------------------  */
/*  04.01.2012 ! Oppolzer    ! neu erstellt                           */
/*  12.01.2012 ! Oppolzer    ! PCODE-ASSEMBLER fertig                 */
/*  15.01.2012 ! Oppolzer    ! erste Befehle werden interpretiert     */
/*  12.11.2016 ! Oppolzer    ! CSP-Tabelle von PASCAL2 uebernommen    */
/*  13.11.2016 ! Oppolzer    ! Interpreter-Meldungen auf stderr       */
/*  19.11.2016 ! Oppolzer    ! etliches laeuft, auch Prozeduraufrufe  */
/*  20.11.2016 ! Oppolzer    ! rekursive Prozeduren sind ok (FIBOK)   */
/*  08.12.2016 ! Oppolzer    ! diverse Portabilitaetprobleme gefixt   */
/*  08.12.2016 ! Oppolzer    ! in Mainframe Version, als Folge davon  */
/*  08.12.2016 ! Oppolzer    ! DEF und XJP neu implementiert          */
/*  17.12.2016 ! Oppolzer    ! Vergleiche auch mit real-Werten        */
/*  22.12.2016 ! Oppolzer    ! Pascal-Compiler laeuft !!!             */
/*  26.12.2016 ! Oppolzer    ! Aufteilung P-Code-Assembler und Intp.  */
/*  27.12.2016 ! Oppolzer    ! FILECB in Pascal-Store abgelegt        */
/*  28.12.2016 ! Oppolzer    ! Binaerfiles funktionieren              */
/*  28.12.2016 ! Oppolzer    ! Zusammenlinken von Modulen             */
/*  28.12.2016 ! Oppolzer    ! long jumps (Spruenge in andere         */
/*  28.12.2016 ! Oppolzer    ! Bloecke, UXJ, XLB)                     */
/*  28.12.2016 ! Oppolzer    ! neuer Typ VOIDPTR (Aend. in PASCAL1)   */
/*  20.01.2017 ! Oppolzer    ! div. GK-Operationen hinzugefuegt       */
/*  20.01.2017 ! Oppolzer    ! Speicherlayout geaend. fuer heap       */
/*  09.02.2017 ! Oppolzer    ! neue CSPs FLR, TRC, RND                */
/*  30.05.2017 ! Oppolzer    ! Terminal-Flag in Pascal-FCB            */
/*  30.05.2017 ! Oppolzer    ! some corrections in Terminal-I/O       */
/*  18.06.2017 ! Oppolzer    ! DFC M and LCA M with opt. type tag     */
/*  10.11.2017 ! Oppolzer    ! new instruction MFI                    */
/*  11.11.2017 ! Oppolzer    ! new instructions MCP, MSE, DBG         */
/*  .......... ! ........    ! .....................................  */
/*  .......... ! ........    ! .....................................  */
/*  .......... ! ........    ! .....................................  */
/*  .......... ! ........    ! .....................................  */
/*                                                                    */
/**********************************************************************/


#include "pcint.h"





static size_t rzt_sysdate (char *dest, int laenge)

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





static size_t rzt_sysdate_usa (char *dest, int laenge)

{
   /************************************************/
   /*   Ermittelt Tagesdatum und Uhrzeit in der    */
   /*   Form mm/tt/jjjj hh:mm:ss                   */
   /************************************************/

   time_t temp;
   struct tm *timeptr;
   size_t result;

   temp = time (NULL);
   timeptr = localtime (&temp);
   result = strftime (dest, laenge,
                      "%m/%d/%Y %H:%M:%S", timeptr);

   return result;
}




/************************************************/
/*   strcmp_ignore                              */
/*                                              */
/*   Vergleichen ohne Gross/Klein-Untersch.     */
/************************************************/

static int strcmp_ignore (char *cp1, char *cp2)

{
   int x;

   for (;;)
   {
      if ((x = toupper(*cp1) - toupper(*cp2)) != 0)
         return x;

      if (*cp1 == 0x00)
         return 0;

      cp1 ++;
      cp2 ++;
   }

   return 0;
}




/************************************************/
/*   memcmp_ignore                              */
/*                                              */
/*   Vergleichen ohne Gross/Klein-Untersch.     */
/************************************************/

static int memcmp_ignore (char *cp1, char *cp2, int laenge)

{
   int x;

   while (laenge > 0)
   {
      if ((x = toupper(*cp1) - toupper(*cp2)) != 0)
         return x;

      cp1 ++;
      cp2 ++;
      laenge --;
   }

   return 0;
}




static void s_toupper (char *s)

{
   while (*s != 0x00)
   {
      *s = toupper (*s);
      s ++;
   }
}






/************************************************/
/*   parse_words                                */
/*                                              */
/*   Worte innerhalb String abgrenzen           */
/************************************************/

static int parse_words (char *zeile,
                        int maxparm,
                        char **p,
                        int *l)
{
   char **plauf;
   int *llauf;
   int i;
   int anz;
   char *cp;

   for (cp = zeile, i = 0, plauf = p, llauf = l;
        i < maxparm;
        i ++, plauf ++, llauf ++)
   {
      while (*cp == ' ')
         cp ++;

      if (*cp == '\n' || *cp == 0x00)
         break;

      *plauf = cp;

      while (*cp != ' ' && *cp != '\n' && *cp != 0x00)
         cp ++;

      *llauf = cp - *plauf;
   }

   anz = i;

   while (i < maxparm)
   {
      *plauf = NULL;
      *llauf = 0;
      i ++;
      plauf ++;
      llauf ++;
   }

   return anz;
}




static size_t ermittle_timestamp (char * dest, int laenge)

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
                      "%Y-%m-%d-%H.%M.%S.000000", timeptr);

   return result;
}




#ifdef HOST
#define PARMFILE   "DD:PARMFILE"
#else
#define PARMFILE   "parmfile.txt"
#endif




static int rzp_scanargs_file (char **parmkeywords,
                              char **parmvalues,
                              int parmanzahl,
                              char **fehlerpos)

/************************************************/
/*   Einlesen Kommandozeilenparameter aus       */
/*   Parameterdatei.                            */
/************************************************/

{
   char **kw;
   char *v;
   int l;
   int lvalue;
   int i;
   int gefunden;

   FILE *parmfile;

   char *zeile;
   char *cp;
   char *cp2;

   if ((parmfile = fopen (PARMFILE, "r")) == NULL)
      return 6;

   zeile = malloc (310);

   while ((cp = fgets (zeile, 300, parmfile)) != NULL)
   {
      if (*cp == '*')
         continue;

      while (*cp == ' ')
         cp ++;

      if (*cp == '\n')
         continue;

      cp2 = cp + strlen (cp) - 1;
      if (*cp2 == '\n')
         *cp2 = 0x00;

      *fehlerpos = cp;
      gefunden = 0;

      for (kw = parmkeywords, i = 0; i < parmanzahl; kw++, i++)
      {
         l = strlen (*kw);

         if (memcmp_ignore (cp, *kw, l) == 0)
         {
            gefunden = 1;
            cp += l;

            cp2 = cp + strlen (cp);
            while (cp2 > cp)
            {
               cp2 --;

               if (*cp2 != 0x00 && *cp2 != '\n' &&
                   *cp2 != 0x0d && *cp2 != ' ')
               {
                  cp2 ++;
                  break;
               }
            }

            lvalue = cp2 - cp;
            v = malloc (lvalue + 1);
            memcpy (v, cp, lvalue);
            v [lvalue] = 0x00;
            parmvalues [i] = v;
            break;
         }
      }

      if (! gefunden)
      {
         fclose (parmfile);
         return 7;
      }
   }

   fclose (parmfile);
   free (zeile);
   *fehlerpos = NULL;

   return 0;
}




/***************************************************/
/*   rzp_scanargs                                  */
/*                                                 */
/*   Aufrufparameter durchsuchen;                  */
/*   Zulaessige Keywords stehen in parmkeywords,   */
/*   Werte sollen nachher in parmvalues stehen.    */
/*   parmmaxlaenge gibt die maximale Laenge        */
/*   der jeweiligen Parameter an.                  */
/*   parmanzahl ist die Anzahl der Parameter.      */
/*                                                 */
/*   Rueckgabe:                                    */
/*                                                 */
/*   0  - falls alles ok.                          */
/*   1  - falls unbekannter Parameter              */
/*   2  - falls Parameter NULL ist                 */
/*   3  - falls Parameter zu lang ist              */
/*   4  - Pruefung 1: Parameter nicht J/N          */
/*   5  - Pruefung 2: Parameter nicht numerisch    */
/*                                                 */
/*   parmposition enthaelt die Parameterposition,  */
/*   entweder in argv oder in parmkeywords,        */
/*   abhaengig von der Fehlerklasse.               */
/***************************************************/

static int rzp_scanargs (int s_argc,
                         char **s_argv,
                         char **parmkeywords,
                         char **parmvalues,
                         int *parmmaxlaenge,
                         int *parmpruefungen,
                         int parmanzahl,
                         int *parmposition,
                         char **parmfehlerpos,
                         char *user_cmdline)

{
   /************************************************/
   /*   Durchsuchen der Kommandozeilenparameter    */
   /*   und ueberpruefen auf Uebereinstimmung mit  */
   /*   parmkeywords; Werte ggf. nach parmvalues.  */
   /************************************************/

   int argc;
   char ** argv;
   char ** kw;
   char ** v;
   int gefunden;
   int l;
   int i;
   int rc;
   int userparm = 0;
   char *cpuser = NULL;

   *parmposition = -1;
   *parmfehlerpos = NULL;

   s_argc --;
   s_argv ++;

   /************************************************/
   /*   Falls es eine Parameterdatei gibt,         */
   /*   wird sie gelesen; die dort eingetragenen   */
   /*   Parameterwerte werden uebernommen.         */
   /************************************************/

   argv = s_argv;
   if (memcmp_ignore (*argv, "parmfile", 8) == 0)
   {
      rc = rzp_scanargs_file (parmkeywords,
                              parmvalues,
                              parmanzahl,
                              parmfehlerpos);
      if (rc != 0)
         return rc;

      s_argc --;
      s_argv ++;
   }

   for (argc = s_argc, argv = s_argv;
        argc > 0;
        argc --, argv ++)
   {
      gefunden = 0;

      if (strcmp (*argv, "/") == 0)
      {
         userparm = argc;
         break;
      }

      for (kw = parmkeywords, v = parmvalues, i = 0;
           i < parmanzahl;
           kw++, v++, i++)
      {
         l = strlen(*kw);

         if (memcmp_ignore (*argv, *kw, l) == 0)
         {
            *v = (*argv) + l;
            gefunden = 1;
         }
      }

      if (! gefunden)
      {
         *parmposition = argv - s_argv + 1;
         return 1;
      }
   }

   if (userparm > 0)
   {
      cpuser = user_cmdline;
      cpuser += strlen (cpuser);

      for (argc = userparm - 1,
             argv = s_argv + s_argc - (userparm -  1);
           argc > 0;
           argc --, argv ++)
      {
         if (cpuser - user_cmdline + strlen (*argv) + 3 < CMDLINEMAX)
         {
            sprintf (cpuser, " %s", *argv);
            cpuser += strlen (cpuser);
         }
      }
   }

   for (v = parmvalues, i = 0;
        i < parmanzahl;
        v++, i++)
   {
      if (*v == NULL)
      {
         *parmposition = i;
         return 2;
      }

      if ((int) (strlen (*v)) > parmmaxlaenge [i])
      {
         *parmposition = i;
         return 3;
      }

      switch (parmpruefungen [i])
      {
         case 1:

            if (**v == 'J' || **v == 'N')
               break;

            if (**v == 'j' || **v == 'Y' || **v == 'y')
            {
               *v = "J";
            }
            else if (**v == 'n')
            {
               *v = "N";
            }
            else
            {
               *parmposition = i;
               return 4;
            }

            break;

         case 2:

            if (strspn (*v, "0123456789") < strlen (*v))
            {
               *parmposition = i;
               return 5;
            }

            break;

         default:
            break;
      }
   }

   return 0;
}




static void parm_fehler (int fnr,
                         void *info1,
                         void *info2,
                         void *info3,
                         void *info4,
                         void *info5,
                         void *info6,
                         void *info7)

/************************************************/
/*   Ausgabe von Laufzeitfehlern,               */
/*   ggf. Programmabbruch                       */
/************************************************/

{
   fprintf (stderr, "\n\n");
   fprintf (stderr, "+++ Fehler in den Aufrufparametern fuer PCINT:\n\n");
   fprintf (stderr, "    ");

   switch (fnr)
   {
      case 1:
         fprintf (stderr, "Unbekannter Parameter \"%s\";\n",
                 (char *) info1);
         break;

      case 2:
         fprintf (stderr, "Wert fuer \"%s\" fehlt;\n",
                 (char *) info1);
         break;

      case 3:
         fprintf (stderr, "Wert fuer \"%s\" hat mehr als %d Zeichen;\n",
                 (char *) info1, *((int *) info2));
         break;

      case 4:
         fprintf (stderr, "Wert fuer Parameter %s unzulaessig "
                  " (sollte 'J' oder 'N' sein);\n",
                  (char *) info1);
         break;

      case 5:
         fprintf (stderr, "Wert fuer Parameter %s unzulaessig "
                  "(nicht numerisch);\n",
                  (char *) info1);
         break;

      case 6:
         fprintf (stderr, "Eingabedatei %s nicht gefunden;\n",
                  (char *) info1);
         break;

      case 7:
         fprintf (stderr, "Unbekannter Parameter \"%s\" im Parameter-File;\n",
                 (char *) info1);
         break;

      case 10:
         fprintf (stderr, "Ausgabedatei %s aufmachen klappt nicht;\n",
                  (char *) info1);
         break;

      default:
         fprintf (stderr, "Unbekannte Fehlernummer\n");
         break;
   }

   fprintf (stderr, "    Abbruch des Moduls.\n\n");

   return;
}




double roundx (double wert, short bereich)

/**********************************************************/
/*                                                        */
/*   roundx.c                                             */
/*                                                        */
/*   Rundungsfunktion neu mit geaenderter Logik;          */
/*   die Korrekturkonstante wird anhand der Groessen-     */
/*   ordnung des Ausgangswertes bestimmt (Ausgangs-       */
/*   wert durch (16 hoch 13); damit wird bei beiden       */
/*   Plattformen mindestens eine 1 an der letzten         */
/*   Ziffernposition dazuaddiert).                        */
/*                                                        */
/*   Autor: Bernd Oppolzer                                */
/*          April 1995                                    */
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

   faktor = fakttab [ 4 - bereich ];

   if ( wert < 0.0 )
      test = -wert;
   else
      test = wert;

   if (test < 1.0e-55)
      return 0.0;

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
      if ( wert < 0.0 )
         return -test / faktor;
      else
         return test / faktor;
   }
   else if (bereich > 0)
   {
      test = (test + rundkonst) / faktor + 0.5;
      test = floor (test);
      if ( wert < 0.0 )
         return -test * faktor;
      else
         return test * faktor;
   }
   else
   {
      test = (test + rundkonst) + 0.5;
      test = floor (test);
      if ( wert < 0.0 )
         return -test;
      else
         return test;
   }
}




static void runtime_error (void *vgs, int errn, char *info)

{
   global_store *gs = vgs;

   fprintf (stderr, "\n\n");
   fprintf (stderr, "+++ runtime error %s\n",
            runtime_errmsg [errn - 1]);
   fprintf (stderr, "    at line %d\n", gs -> lineofcode);
   if (info != NULL)
   {
      fprintf (stderr, "    error info = %s\n", info);
   }
   fprintf (stderr, "\n\n");

   fprintf (stdout, "\n\n");
   fprintf (stdout, "+++ runtime error %s\n",
            runtime_errmsg [errn - 1]);
   fprintf (stdout, "    at line %d\n", gs -> lineofcode);
   if (info != NULL)
   {
      fprintf (stdout, "    error info = %s\n", info);
   }
   fprintf (stdout, "\n\n");

   exit (1000);
}




static int next_fcb (void *vgs)

{
   global_store *gs = vgs;
   int x;

   if (gs -> actfiles >= gs -> maxfiles)
      runtime_error (gs, TOOMUCHFILES, NULL);

   x = gs -> actfilepos;
   gs -> actfilepos += sizeof (filecb);
   (gs -> actfiles) ++;

   memset (gs -> store0 + x, INIT_PATTERN, sizeof (filecb));

   return x;
}




static int store_alloc (void *vgs, int length)

{
   global_store *gs = vgs;
   int x;

   if (gs -> actnewheap + length >=
       gs -> firstnewheap + gs -> sizenewheap)
   {
      runtime_error (gs, NOMOREHEAP, NULL);
   }

   x = gs -> actnewheap;
   gs -> actnewheap += length;

   memset (gs -> store0 + x, INIT_PATTERN, length);

   return x;
}




static void file_input (void *vgs, filecb *fcb)

{
   global_store *gs = vgs;
   char *cp;
   char *charp;

   //*********************************************
   // Zeile einlesen
   //*********************************************

   cp = fgets (fcb -> fbuffer, fcb -> fbuflen - 2, fcb -> fhandle);
   if (cp == NULL)
   {
      fcb -> eoln = 1;
      fcb -> eof = 1;

      charp = ADDRSTOR (fcb -> pfilvar);
      *charp = ' ';

      return;
   }

   //*********************************************
   // Pruefen, ob Zeile komplett reingepasst hat
   //*********************************************

   cp = fcb -> fbuffer + strlen (fcb -> fbuffer) - 1;
   if (*cp == '\n')
   {
      //*********************************************
      // checken wg. Linux ...
      // Zeilenende durch Blank ersetzen, freclen setzen
      //*********************************************

      if (cp > fcb -> fbuffer)
      {
         cp --;
         if (*cp != 0x0d)
            cp ++;
      }

      *cp = ' ';
   }
   else
   {
      cp ++;
   }

   fcb -> freclen = cp - fcb -> fbuffer;
   fcb -> eoln = 0;
   fcb -> eof = 0;

   //*********************************************
   // Bufferpointer und Filevariable setzen
   //*********************************************

   fcb -> fbufptr = fcb -> fbuffer;

   charp = ADDRSTOR (fcb -> pfilvar);
   *charp = *(fcb -> fbufptr);

   if (fcb -> fbufptr - fcb -> fbuffer >= fcb -> freclen)
      fcb -> eoln = 1;

   return;
}




static char file_getch (void *vgs, filecb *fcb)

{
   global_store *gs = vgs;
   char ch;
   char *charp;
   int *intp;

   charp = ADDRSTOR (fcb -> pfilvar);
   ch = *charp;

   if (fcb -> eoln)
   {
      if (fcb -> terminal != 'Y')
      {
         file_input (gs, fcb);
      }
   }
   else
   {
      fcb -> fbufptr += 1;

      charp = ADDRSTOR (fcb -> pfilvar);
      *charp = *(fcb -> fbufptr);

      if (fcb -> fbufptr - fcb -> fbuffer >= fcb -> freclen)
         fcb -> eoln = 1;
   }

   return ch;
}




static char file_getch_noblank (void *vgs, filecb *fcb)

{
   global_store *gs = vgs;
   char ch;

   for (;;)
   {
      ch = file_getch (gs, fcb);

      if (ch != ' ' || fcb -> eof != 0)
         break;

      if (fcb -> eoln)
      {
         file_input (gs, fcb);
      }
   }

   return ch;
}




static FILE *file_open (filecb *fcb, char *fmode)

{
   FILE *f;
   char envbuffer [20];
   char *fname;
   char *fname_neu = NULL;

   if (fcb -> terminal == 'Y')
   {
      if (*fmode == 'r')
      {
         f = stdin;
      }
      else
      {
         f = stdout;
      }

      return f;
   }

   if (*(fcb -> filename) != 0x00)
   {
      fname_neu = fcb -> filename;
   }
   else
   {
      fname = fcb -> ddname;
      sprintf (envbuffer, "DD_%s", fcb -> ddname);
      fname_neu = getenv (envbuffer);
   }

   if (fname_neu != NULL)
   {
      if (strcmp (fname_neu, "*stdin*") == 0)
      {
         f = stdin;
         fcb -> terminal = 'Y';
      }
      else if (strcmp (fname_neu, "*stdout*") == 0)
      {
         f = stdout;
         fcb -> terminal = 'Y';
      }
      else
      {
         f = fopen (fname_neu, fmode);
         fcb -> terminal = 'N';
      }

      return f;
   }

   if (strcmp (fname, "INPUT") == 0)
   {
      f = stdin;
      fcb -> terminal = 'Y';
   }
   else if (strcmp (fname, "OUTPUT") == 0)
   {
      f = stdout;
      fcb -> terminal = 'Y';
   }
   else
   {
      f = fopen (fname, fmode);
      fcb -> terminal = 'N';
   }

   return f;
}




static void check_read (void *vgs, filecb *fcb)

{
   global_store *gs = vgs;
   int doread = 0;
   char *charp;

   switch (fcb -> status)
   {
      case '0':
         if (fcb -> textfile)
         {
            if (fcb -> fhandle != NULL)
               fclose (fcb -> fhandle);
            if (fcb -> fbuffer != NULL)
               free (fcb -> fbuffer);

            fcb -> fhandle = file_open (fcb, "r");
            if (fcb -> fhandle == NULL)
               runtime_error (gs, FILENOTFOUND, fcb -> ddname);
            fcb -> fbuflen = 65535;
            fcb -> fbuffer = malloc (fcb -> fbuflen);
            fcb -> pfilvar = fcb -> pstore + 8;
            fcb -> status = '1';
            doread = 1;
         }
         else
         {
            if (fcb -> fhandle != NULL)
               fclose (fcb -> fhandle);
            if (fcb -> fbuffer != NULL)
               free (fcb -> fbuffer);

            fcb -> fhandle = file_open (fcb, "rb");
            if (fcb -> fhandle == NULL)
               runtime_error (gs, FILENOTFOUND, fcb -> ddname);
            fcb -> fbuflen = 0;
            fcb -> fbuffer = NULL;
            fcb -> pfilvar = fcb -> pstore + 8;
            fcb -> status = '1';
            doread = 1;
         }
         break;

      case '1':
         if (fcb -> textfile)
            doread = 1;
         break;

      case '2':
         if (fcb -> inout == 'O')
            runtime_error (gs, BADFILE, fcb -> ddname);
         if (fcb -> fhandle != NULL)
            fclose (fcb -> fhandle);
         if (fcb -> fbuffer != NULL)
            free (fcb -> fbuffer);

         if (fcb -> textfile)
         {
            fcb -> fhandle = file_open (fcb, "r");
            if (fcb -> fhandle == NULL)
               runtime_error (gs, FILENOTFOUND, fcb -> ddname);
            fcb -> fbuflen = 65535;
            fcb -> fbuffer = malloc (fcb -> fbuflen);
            fcb -> pfilvar = fcb -> pstore + 8;
            fcb -> status = '1';
            doread = 1;
         }
         else
         {
            fcb -> fhandle = file_open (fcb, "rb");
            if (fcb -> fhandle == NULL)
               runtime_error (gs, FILENOTFOUND, fcb -> ddname);
            fcb -> fbuflen = 0;
            fcb -> fbuffer = NULL;
            fcb -> pfilvar = fcb -> pstore + 8;
            fcb -> status = '1';
            doread = 1;
         }
         break;

      case '3':
         if (fcb -> readbuf_sched == 'Y')
         {
            fcb -> readbuf_sched = ' ';
            doread = 1;
         }
         break;

      case '4':
         runtime_error (gs, BADFILE, fcb -> ddname);
         break;

      default:
         break;
   }

   if (doread)
   {
      if (fcb -> textfile)
      {
         file_input (gs, fcb);
         fcb -> status = '3';
      }
      else
      {
         int x;

         charp = ADDRSTOR (fcb -> pfilvar);
         x = fread (charp, fcb -> freclen, 1, fcb -> fhandle);
         fcb -> eof = (x == 0);
         fcb -> status = '3';
      }
   }
}




static void check_write (void *vgs, filecb *fcb)

{
   global_store *gs = vgs;

   switch (fcb -> status)
   {
      case '0':
         if (fcb -> fhandle != NULL)
            fclose (fcb -> fhandle);
         if (fcb -> fbuffer != NULL)
            free (fcb -> fbuffer);

         if (fcb -> textfile)
         {
            fcb -> fhandle = file_open (fcb, "w");
            fcb -> pfilvar = fcb -> pstore + 8;
            fcb -> status = '4';
         }
         else
         {
            fcb -> fhandle = file_open (fcb, "wb");
            fcb -> pfilvar = fcb -> pstore + 8;
            fcb -> status = '4';
         }
         break;

      case '1':
         if (fcb -> inout == 'I')
            runtime_error (gs, BADFILE, fcb -> ddname);
         if (fcb -> fhandle != NULL)
            fclose (fcb -> fhandle);
         if (fcb -> fbuffer != NULL)
            free (fcb -> fbuffer);

         if (fcb -> textfile)
         {
            fcb -> fhandle = file_open (fcb, "w");
            fcb -> pfilvar = fcb -> pstore + 8;
            fcb -> status = '4';
         }
         else
         {
            fcb -> fhandle = file_open (fcb, "wb");
            fcb -> pfilvar = fcb -> pstore + 8;
            fcb -> status = '4';
         }
         break;

      case '2':
         fcb -> status = '4';
         break;

      case '3':
         runtime_error (gs, BADFILE, fcb -> ddname);
         break;

      default:
         break;
   }
}




static int used_millisecs (int init)

{
   clock_t clockx;
   double sec;
   int sec_return;
   static int sec_init = 0;

   clockx = clock ();
   sec = clockx;
   sec /= CLOCKS_PER_SEC;
   sec *= 1000;
   sec_return = sec;

   if (init)
      sec_init = sec_return;
   else
      sec_return = sec_return - sec_init;

   return sec_return;
}




static void *cspf_dat (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;

   rzt_sysdate_usa (gs -> date_string, 25);
   memcpy (ADDRSTOR (328), gs -> date_string, 10);

   return NULL;
}




static void *cspf_tim (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;

   rzt_sysdate_usa (gs -> date_string, 25);
   memcpy (ADDRSTOR (338), gs -> date_string + 11, 10);

   return NULL;
}




static void *cspf_rnd (void *vgs,
                       double *parm1)

{
   global_store *gs = vgs;
   double dwert;
   int *intp;

   if (parm1 == NULL)
   {
      parm1 = ADDRSTACK ((gs -> sp) - 4);
   }

#if 0

   fprintf (stderr, "rnd: *parm1 = %e\n", *parm1);

#endif

   STACKTYPE (gs -> sp) = ' ';

   dwert = *parm1;

   (gs -> sp) -= 4;
   STACKTYPE (gs -> sp) = ' ';

   intp = ADDRSTACK (gs -> sp);
   *intp = roundx (dwert, 0);

   return NULL;
}




static void *cspf_trc (void *vgs,
                       double *parm1)

{
   global_store *gs = vgs;
   double dwert;
   int *intp;

   if (parm1 == NULL)
   {
      parm1 = ADDRSTACK ((gs -> sp) - 4);
   }

#if 0

   fprintf (stderr, "trc: *parm1 = %e\n", *parm1);

#endif

   STACKTYPE (gs -> sp) = ' ';

   dwert = *parm1;

   (gs -> sp) -= 4;
   STACKTYPE (gs -> sp) = ' ';

   intp = ADDRSTACK (gs -> sp);
   if (dwert >= 0.0)
   {
      *intp = (int) (floor (dwert));
   }
   else
   {
      *intp = (int) (- floor (- dwert));
   }

   return NULL;
}




static void *cspf_flr (void *vgs,
                       double *parm1)

{
   global_store *gs = vgs;

   if (parm1 == NULL)
   {
      parm1 = ADDRSTACK ((gs -> sp) - 4);
   }

#if 0

   fprintf (stderr, "flr: *parm1 = %e\n", *parm1);

#endif

   *parm1 = floor (*parm1);

   return NULL;
}




static void *cspf_xit (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;

#if 0

   fprintf (stderr, "xit: parm1 = %d\n", parm1);

#endif

   fprintf (stdout, "\n*** EXIT Aufruf mit Parameter = %d ***\n",
            parm1);

   gs -> stepanz = -10;

   return NULL;
}




static void *cspf_clk (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   int secs;
   int *intp;

#if 0

   fprintf (stderr, "clk: parm1 = %d\n", parm1);

#endif

   secs = used_millisecs (0);

   intp = ADDRSTACK (gs -> sp);
   *intp = secs;

   return NULL;
}




static void *cspf_cls (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;

#if 0

   fprintf (stderr, "cls: parm1 = %d\n", parm1);

#endif

   STOR_FCB (parm1, fcb);

   if (fcb -> fhandle != NULL)
   {
      //****************************************************
      //   important: don't close std files
      //****************************************************

      if (fcb -> fhandle != stdin &&
          fcb -> fhandle != stdout &&
          fcb -> fhandle != stderr)
      {
         fclose (fcb -> fhandle);
      }

      fcb -> fhandle = NULL;
   }

   if (fcb -> fbuffer != NULL)
   {
      free (fcb -> fbuffer);
      fcb -> fbuffer = NULL;
      fcb -> fbuflen = 0;
   }

   fcb -> status = '0';

   return NULL;
}




static void *cspf_res (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;

#if 0

   fprintf (stderr, "res: parm1 = %d\n", parm1);

#endif

   STOR_FCB (parm1, fcb);

   fcb -> status = '0';

   check_read (gs, fcb);

   return NULL;
}




static void *cspf_rew (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;

#if 0

   fprintf (stderr, "rew: parm1 = %d\n", parm1);

#endif

   STOR_FCB (parm1, fcb);

   fcb -> status = '0';

   check_write (gs, fcb);

   fcb -> status = '2';

   return NULL;
}




static void *cspf_wrd (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;
   char *charp;
   char *charp2;

#if 0

   fprintf (stderr, "wrd: parm1 = %d\n", parm1);
   fprintf (stderr, "wrd: parm2 = %d\n", parm2);
   fprintf (stderr, "wrd: parm3 = %d\n", parm3);

#endif

   STOR_FCB (parm1, fcb);

   check_write (gs, fcb);

   charp = ADDRSTOR (parm2);
   charp2 = ADDRSTOR (fcb -> pfilvar);

   memcpy (charp2, charp, parm3);
   fwrite (charp, parm3, 1, fcb -> fhandle);

   return NULL;
}




static void *cspf_wrc (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;

#if 0

   fprintf (stderr, "wrc: parm1 = %d\n", parm1);
   fprintf (stderr, "wrc: parm2 = %d\n", parm2);
   fprintf (stderr, "wrc: parm3 = %d\n", parm3);

#endif

   STOR_FCB (parm1, fcb);

   check_write (gs, fcb);

   if (parm3 > 1)
      fprintf (fcb -> fhandle, "%*c", parm3, parm2);
   else if (parm3 > 0)
      fprintf (fcb -> fhandle, "%c", parm2);

   return NULL;
}




static void *cspf_wre (void *vgs,
                       int parm1,
                       double parmd,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;
   char *charp;
   char *charp2;

#if 0

   fprintf (stderr, "wre: parm1 = %d\n", parm1);
   fprintf (stderr, "wrr: parmd = %15.7f\n", parmd);
   fprintf (stderr, "wre: parm3 = %d\n", parm3);

#endif

   STOR_FCB (parm1, fcb);

   check_write (gs, fcb);

   if (parm3 <= 0)
      return NULL;

   charp2 = ADDRSTOR (fcb -> pfilvar);

   memcpy (charp2, &parmd, parm3);
   fwrite (charp, parm3, 1, fcb -> fhandle);

   return NULL;
}




static void *cspf_wrr (void *vgs,
                       int parm1,
                       double parmd,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;
   char buffer [256];

#if 0

   fprintf (stderr, "wrr: parm1 = %d\n", parm1);
   fprintf (stderr, "wrr: parmd = %15.7f\n", parmd);
   fprintf (stderr, "wrr: parm3 = %d\n", parm3);
   fprintf (stderr, "wrr: parm4 = %d\n", parm4);

#endif

   STOR_FCB (parm1, fcb);

   check_write (gs, fcb);

   if (parm3 <= 0)
      return NULL;

   if (parm4 >= 0)
   {
      //**********************************************
      // roundx rounding is needed, because otherwise
      // the sprintf rounding is not accurate enough
      // see article on Facebook page
      //**********************************************

      if (parm4 <= 15)
         parmd = roundx (parmd, - parm4);

      sprintf (buffer, "%*.*f", parm3, parm4, parmd);
   }
   else
   {
      if (parm3 >= 8)
         sprintf (buffer, "%*.*E", parm3, parm3 - 8, parmd);
      else
         sprintf (buffer, "%*E", parm3, parmd);
   }

   fprintf (fcb -> fhandle, "%s", buffer);

#if 0

   fprintf (stderr, "wrr: stdout = %p\n", stdout);
   fprintf (stderr, "wrr: fcb -> fhandle = %p\n", fcb -> fhandle);
   fprintf (stderr, "wrr: buffer = %s\n", buffer);

#endif

   return NULL;
}




static void *cspf_wrs (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   char *charp;
   filecb *fcb;

#if 0

   fprintf (stderr, "wrs: parm1 = %d\n", parm1);
   fprintf (stderr, "wrs: parm2 = %d\n", parm2);
   fprintf (stderr, "wrs: parm3 = %d\n", parm3);
   fprintf (stderr, "wrs: parm4 = %d\n", parm4);

#endif

   STOR_FCB (parm1, fcb);

   check_write (gs, fcb);

   if (parm3 <= 0)
      return NULL;

   //*******************************************************
   // wenn parm3 > parm4, blanks in entsprechender anzahl ausg.
   //*******************************************************

   if (parm3 > parm4)
      fprintf (fcb -> fhandle, "%*c", parm4 - parm3, ' ');

   //*******************************************************
   // 2. parameter = adresse des strings
   //*******************************************************

   charp = ADDRSTOR (parm2);

   //*******************************************************
   // wenn parm3 < parm4, nur in laenge parm3 ausgeben
   //*******************************************************

   if (parm3 < parm4)
      parm4 = parm3;

   fprintf (fcb -> fhandle, "%-*.*s", parm4, parm4, charp);

   return NULL;
}




static void *cspf_wrx (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   short *shortp;
   char *charp;
   filecb *fcb;
   short maxval;
   char buf [31];
   int len;
   int offs;

#if 0

   fprintf (stderr, "wrx: parm1 = %d\n", parm1);
   fprintf (stderr, "wrx: parm2 = %d\n", parm2);
   fprintf (stderr, "wrx: parm3 = %d\n", parm3);
   fprintf (stderr, "wrx: parm4 = %d\n", parm4);

#endif

   STOR_FCB (parm1, fcb);

   check_write (gs, fcb);

   if (parm3 <= 0)
      return NULL;

   shortp = ADDRSTOR (parm4);
   charp = ADDRSTOR (parm4);
   maxval = *shortp;
   maxval = maxval / 4 - 1;

   if (parm2 < 0 || parm2 > maxval)
   {
      sprintf (buf, "WRX:%04hd", parm2);
      len = strlen (buf);
   }
   else
   {
      parm2 = maxval - parm2;
      offs = shortp [parm2 * 2];
      len = shortp [parm2 * 2 + 1];
      memcpy (buf, charp + offs, len);
      buf [len] = 0x00;
   }

#if 0

   fprintf (stderr, "wrx: offs = %d\n", offs);
   fprintf (stderr, "wrx: len = %d\n", len);

#endif

   if (parm3 > len)
      fprintf (fcb -> fhandle, "%*c", parm3 - len, ' ');

   fprintf (fcb -> fhandle, "%-*.*s", len, len, buf);

   return NULL;
}




static void *cspf_wrp (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;
   char buf [9];

#if 0

   fprintf (stderr, "wrp: parm1 = %d\n", parm1);
   fprintf (stderr, "wrp: parm2 = %d\n", parm2);
   fprintf (stderr, "wrp: parm3 = %d\n", parm3);

#endif

   STOR_FCB (parm1, fcb);

   check_write (gs, fcb);

   if (parm3 <= 0)
      return NULL;

   if (parm3 > 8)
      fprintf (fcb -> fhandle, "%*c", parm3 - 8, ' ');

   if (parm2 == -1)
      sprintf (buf, "%-*.*s", parm3, parm3, "nil");
   else
      sprintf (buf, "%*p", parm3, ADDRSTOR (parm2));

   fprintf (fcb -> fhandle, "%s", buf);

   return NULL;
}




static void *cspf_wrb (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;
   char buf [6];

#if 0

   fprintf (stderr, "wrb: parm1 = %d\n", parm1);
   fprintf (stderr, "wrb: parm2 = %d\n", parm2);
   fprintf (stderr, "wrb: parm3 = %d\n", parm3);

#endif

   STOR_FCB (parm1, fcb);

   check_write (gs, fcb);

   if (parm3 <= 0)
      return NULL;

   if (parm3 > 5)
      fprintf (fcb -> fhandle, "%*c", parm3 - 5, ' ');

   if (parm2 == 0)
      strcpy (buf, "FALSE");
   else if (parm2 == 1)
      strcpy (buf, "TRUE ");
   else
      strcpy (buf, "WRB:?");

   fprintf (fcb -> fhandle, "%-*.*s", parm3, parm3, buf);

   return NULL;
}




static void *cspf_wln (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;

#if 0

   fprintf (stderr, "wln: stdout = %p\n", stdout);
   fprintf (stderr, "wln: parm1 = %d\n", parm1);

#endif

   STOR_FCB (parm1, fcb);

   check_write (gs, fcb);

   fprintf (fcb -> fhandle, "\n");

   return NULL;
}




static void *cspf_sio (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{

#if 0

   fprintf (stderr, "sio: parm1 = %d\n", parm1);

#endif

   return NULL;
}




static void *cspf_fdf (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   char *charp;
   int adrfcb;
   filecb *fcb;
   char *cp;
   int *intp;

#if 0

   fprintf (stderr, "fdf: parm1 = %d\n", parm1);
   fprintf (stderr, "fdf: parm2 = %d\n", parm2);
   fprintf (stderr, "fdf: parm3 = %d\n", parm3);

#endif

   adrfcb = next_fcb (gs);
   fcb = (filecb *) (gs -> store0 + adrfcb);

   if (parm3 == 0)
   {
      memcpy (fcb, &nullfcb_text, sizeof (filecb));
   }
   else
   {
      memcpy (fcb, &nullfcb_bin, sizeof (filecb));
      fcb -> freclen = parm3;
   }

   charp = ADDRSTOR (parm2);
   memcpy (fcb -> ddname, charp, 8);
   fcb -> ddname [8] = 0x00;
   cp = fcb -> ddname + 7;
   while (*cp == ' ')
   {
      *cp = 0x00;
      cp --;
   }

#if 0

   fprintf (stderr, "fdf: ddname = %s\n", fcb -> ddname);

#endif

   fcb -> inout = 'U';
   fcb -> pstore = parm1;
   fcb -> terminal = 'N';
   intp = ADDRSTOR (parm1);
   *intp = adrfcb;
   intp ++;
   *intp = parm3;  // Satzlaenge bei Binaerfiles

   return NULL;
}




static void *cspf_eio (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{

#if 0

   fprintf (stderr, "eio: parm1 = %d\n", parm1);

#endif

   return NULL;
}




static void *cspf_get (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;
   char *charp;
   int x;

#if 0

   fprintf (stderr, "get: parm1 = %d\n", parm1);

#endif

   STOR_FCB (parm1, fcb);

   check_read (gs, fcb);

   if (fcb -> textfile)
   {
      file_getch (gs, fcb);
   }
   else
   {
      charp = ADDRSTOR (fcb -> pfilvar);
      x = fread (charp, fcb -> freclen, 1, fcb -> fhandle);
      if (x == 0)
         fcb -> eof = 1;
   }

   return NULL;
}




static void *cspf_put (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;
   char *charp;
   int x;

#if 0

   fprintf (stderr, "put: parm1 = %d\n", parm1);

#endif

   STOR_FCB (parm1, fcb);

   check_write (gs, fcb);

   //***********************************************
   //* put schreibt die File Variable
   //* (im Fall Textfile 1 char)
   //***********************************************

   if (fcb -> textfile)
   {
      charp = ADDRSTOR (fcb -> pfilvar);
      fputc (*charp, fcb -> fhandle);
   }
   else
   {
      charp = ADDRSTOR (fcb -> pfilvar);
      fwrite (charp, fcb -> freclen, 1, fcb -> fhandle);
   }

   return NULL;
}




static void *cspf_eol (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;
   int *intp;
   char *charp;
   char ch;

#if 0

   fprintf (stderr, "eol: parm1 = %d\n", parm1);

#endif

   STOR_FCB (parm1, fcb);

   if (fcb -> status == '0')
   {
      check_read (gs, fcb);
   }

   if (fcb -> eof)
      return NULL;

   intp = ADDRSTACK (gs -> sp);

   for (;;)
   {
      if (fcb -> eoln != 0)
      {
         *intp = 1;
         break;
      }

      charp = ADDRSTOR (fcb -> pfilvar);
      ch = *charp;

      if (ch != ' ')
      {
         *intp = 0;
         break;
      }

      file_getch (gs, fcb);
   }

   return NULL;
}




static void *cspf_eot (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;
   int *intp;

#if 1

   fprintf (stderr, "eot: parm1 = %d\n", parm1);

#endif

   STOR_FCB (parm1, fcb);

   if (fcb -> status == '0')
   {
      check_read (gs, fcb);
   }

   intp = ADDRSTACK (gs -> sp);
   *intp = fcb -> eof;

   fprintf (stderr, "+++ eot noch nicht korrekt implementiert !!\n");

   return NULL;
}




static void *cspf_eof (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;
   int *intp;

#if 0

   fprintf (stderr, "eof: parm1 = %d\n", parm1);

#endif

   STOR_FCB (parm1, fcb);

   if (fcb -> status == '0')
   {
      check_read (gs, fcb);
   }

   if (fcb -> status == '2' ||
       fcb -> status == '4')
   {
      intp = ADDRSTACK (gs -> sp);
      *intp = 1;
   }
   else
   {
      intp = ADDRSTACK (gs -> sp);
      *intp = fcb -> eof;
   }

   return NULL;
}




static void *cspf_eln (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;
   int *intp;

#if 0

   fprintf (stderr, "eln: parm1 = %d\n", parm1);

#endif

   STOR_FCB (parm1, fcb);

   if (fcb -> status == '0')
   {
      check_read (gs, fcb);
   }

   if (fcb -> status == '2' ||
       fcb -> status == '4')
   {
      intp = ADDRSTACK (gs -> sp);
      *intp = 0;
   }
   else
   {
      intp = ADDRSTACK (gs -> sp);
      *intp = fcb -> eoln;
   }

   return NULL;
}




static void *cspf_wri (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;

#if 0

   fprintf (stderr, "wri: parm1 = %d\n", parm1);
   fprintf (stderr, "wri: parm2 = %d\n", parm2);
   fprintf (stderr, "wri: parm3 = %d\n", parm3);

#endif

   STOR_FCB (parm1, fcb);

   check_write (gs, fcb);

   if (parm3 == 0)
      return NULL;

   if (parm3 < 0)
      fprintf (fcb -> fhandle, "%0*d", - parm3, parm2);
   else
      fprintf (fcb -> fhandle, "%*d", parm3, parm2);

   return NULL;
}




static void *cspf_rln (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;
   char *charp;

#if 0

   fprintf (stderr, "rln: parm1 = %d\n", parm1);

#endif

   STOR_FCB (parm1, fcb);

   check_read (gs, fcb);

   if (fcb -> status != '3' || fcb -> eof)
      runtime_error (gs, BADIO, fcb -> ddname);

   if (fcb -> terminal == 'Y')
   {
      fcb -> fbufptr = fcb -> fbuffer + fcb -> freclen;
      fcb -> eoln = 1;

      charp = ADDRSTOR (fcb -> pfilvar);
      *charp = ' ';

      fcb -> readbuf_sched = 'Y';
   }
   else
   {
      file_input (gs, fcb);
   }

   return NULL;
}




static void *cspf_rds (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;
   char ch;
   char *charp;
   int i;

#if 0

   fprintf (stderr, "rds: parm1 = %d\n", parm1);
   fprintf (stderr, "rds: parm2 = %d\n", parm2);
   fprintf (stderr, "rds: parm3 = %d\n", parm3);

#endif

   STOR_FCB (parm1, fcb);

   check_read (gs, fcb);

   if (fcb -> status != '3' || fcb -> eof)
      runtime_error (gs, BADIO, fcb -> ddname);

   //*************************************
   // Pointer to target from parm2
   //*************************************

   charp = ADDRSTOR (parm2);

   //*************************************
   // Init to blanks - length = parm3
   //*************************************

   memset (charp, ' ', parm3);

   for (i = 0; i < parm3; i ++)
   {
      if (fcb -> eof || fcb -> eoln)
         break;

      ch = file_getch (gs, fcb);
      *charp = ch;
      charp ++;
   }

   return NULL;
}




static void *cspf_rdr (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;
   int minus = 0;
   char ch;
   char chnext;
   double wert;
   double wert2;
   double scale;
   int wertexp;
   double *doublep;
   char *charp;
   int minusexp;
   int i;

#if 0

   fprintf (stderr, "rdr: parm1 = %d\n", parm1);
   fprintf (stderr, "rdr: parm2 = %d\n", parm2);

#endif

   STOR_FCB (parm1, fcb);

   check_read (gs, fcb);

   if (fcb -> status != '3' || fcb -> eof)
      runtime_error (gs, BADIO, fcb -> ddname);

   ch = file_getch_noblank (gs, fcb);

   if (fcb -> eof)
      runtime_error (gs, BADIO, fcb -> ddname);

   if (ch == '-')
   {
      minus = 1;
      ch = file_getch_noblank (gs, fcb);
      if (fcb -> eof)
         runtime_error (gs, BADIO, fcb -> ddname);
   }

   if (ch < '0' || ch > '9')
      runtime_error (gs, BADIO, fcb -> ddname);

   wert = (double) (ch - '0');

   do
   {
      charp = ADDRSTOR (fcb -> pfilvar);
      chnext = *charp;
      if (chnext < '0' || chnext > '9')
         break;
      ch = file_getch (gs, fcb);
      wert *= 10.0;
      wert += (double) (ch - '0');
   }
   while (1);

   if (chnext == '.')
   {
      ch = file_getch (gs, fcb);
      wert2 = 0.0;
      scale = 1.0;

      do
      {
         charp = ADDRSTOR (fcb -> pfilvar);
         chnext = *charp;
         if (chnext < '0' || chnext > '9')
            break;
         ch = file_getch (gs, fcb);
         wert2 *= 10.0;
         wert2 += (double) (ch - '0');
         scale *= 10.0;
      }
      while (1);

      wert += (wert2 / scale);
   }

   if (chnext == 'e' || chnext == 'E')
   {
      ch = file_getch (gs, fcb);
      minusexp = 0;
      wertexp = 0.0;

      if (chnext == '-')
      {
         minusexp = 1;
         ch = file_getch (gs, fcb);
      }
      else if (chnext == '+')
      {
         ch = file_getch (gs, fcb);
      }

      if (chnext < '0' || chnext > '9')
         runtime_error (gs, BADIO, fcb -> ddname);

      do
      {
         charp = ADDRSTOR (fcb -> pfilvar);
         chnext = *charp;
         if (chnext < '0' || chnext > '9')
            break;
         ch = file_getch (gs, fcb);
         wertexp *= 10;
         wertexp += (ch - '0');
      }
      while (1);

      if (minusexp)
      {
         for (i = 1; i <= wertexp; i ++)
            wert /= 10.0;
      }
      else
      {
         for (i = 1; i <= wertexp; i ++)
            wert *= 10.0;
      }
   }

   doublep = ADDRSTOR (parm2);

   if (minus)
      (* doublep) = - wert;
   else
      (* doublep) = wert;

   return NULL;
}




static void *cspf_rdh (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;
   int minus = 0;
   char ch;
   char chnext;
   int wert;
   short *shortp;
   char *charp;

#if 0

   fprintf (stderr, "rdh: parm1 = %d\n", parm1);
   fprintf (stderr, "rdh: parm2 = %d\n", parm2);

#endif

   STOR_FCB (parm1, fcb);

   check_read (gs, fcb);

   if (fcb -> status != '3' || fcb -> eof)
      runtime_error (gs, BADIO, fcb -> ddname);

   ch = file_getch_noblank (gs, fcb);

   if (fcb -> eof)
      runtime_error (gs, BADIO, fcb -> ddname);

   if (ch == '-')
   {
      minus = 1;
      ch = file_getch_noblank (gs, fcb);
      if (fcb -> eof)
         runtime_error (gs, BADIO, fcb -> ddname);
   }

   if (ch < '0' || ch > '9')
      runtime_error (gs, BADIO, fcb -> ddname);

   wert = ch - '0';

   do
   {
      charp = ADDRSTOR (fcb -> pfilvar);
      chnext = *charp;
      if (chnext < '0' || chnext > '9')
         break;
      ch = file_getch (gs, fcb);
      wert *= 10;
      wert += ch - '0';
   }
   while (1);

   shortp = ADDRSTOR (parm2);

   if (minus)
      (* shortp) = - wert;
   else
      (* shortp) = wert;

   return NULL;
}




static void *cspf_rdb (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;
   char ch;
   char *charp;

#if 0

   fprintf (stderr, "rdb: parm1 = %d\n", parm1);
   fprintf (stderr, "rdb: parm2 = %d\n", parm2);

#endif

   STOR_FCB (parm1, fcb);

   check_read (gs, fcb);

   if (fcb -> status != '3' || fcb -> eof)
      runtime_error (gs, BADIO, fcb -> ddname);

   ch = file_getch_noblank (gs, fcb);

   if (fcb -> eof)
      runtime_error (gs, BADIO, fcb -> ddname);

   ch = toupper (ch);

   if (ch != 'T' && ch != 'F')
      runtime_error (gs, BADBOOL, fcb -> ddname);

   charp = ADDRSTOR (parm2);
   (* charp) = (ch == 'T');

   return NULL;
}




static void *cspf_rdi (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;
   int minus = 0;
   char ch;
   char chnext;
   int wert;
   int *intp;
   char *charp;

#if 0

   fprintf (stderr, "rdi: parm1 = %d\n", parm1);
   fprintf (stderr, "rdi: parm2 = %d\n", parm2);

#endif

   STOR_FCB (parm1, fcb);

   check_read (gs, fcb);

   if (fcb -> status != '3' || fcb -> eof)
      runtime_error (gs, BADIO, fcb -> ddname);

   ch = file_getch_noblank (gs, fcb);

   if (fcb -> eof)
      runtime_error (gs, BADIO, fcb -> ddname);

   if (ch == '-')
   {
      minus = 1;
      ch = file_getch_noblank (gs, fcb);
      if (fcb -> eof)
         runtime_error (gs, BADIO, fcb -> ddname);
   }

   if (ch < '0' || ch > '9')
      runtime_error (gs, BADIO, fcb -> ddname);

   wert = ch - '0';

   do
   {
      charp = ADDRSTOR (fcb -> pfilvar);
      chnext = *charp;
      if (chnext < '0' || chnext > '9')
         break;
      ch = file_getch (gs, fcb);
      wert *= 10;
      wert += ch - '0';
   }
   while (1);

   intp = ADDRSTOR (parm2);

   if (minus)
      (* intp) = - wert;
   else
      (* intp) = wert;

   return NULL;
}




static void *cspf_rdc (void *vgs,
                       int parm1,
                       int parm2,
                       int parm3,
                       int parm4)

{
   global_store *gs = vgs;
   filecb *fcb;
   char ch;
   char *charp;

#if 0

   fprintf (stderr, "rdc: parm1 = %d\n", parm1);
   fprintf (stderr, "rdc: parm2 = %d\n", parm2);

#endif

   STOR_FCB (parm1, fcb);

   check_read (gs, fcb);

   if (fcb -> status != '3' || fcb -> eof)
      runtime_error (gs, BADIO, fcb -> ddname);

   ch = file_getch (gs, fcb);

   charp = ADDRSTOR (parm2);
   (* charp) = ch;

   return NULL;
}




/**********************************************************/
/*                                                        */
/*   CSP-Table / Stand 2016                               */
/*                                                        */
/**********************************************************/


#define CSP_CTR     0
#define CSP_N01     1
#define CSP_N02     2
#define CSP_N03     3
#define CSP_N04     4
#define CSP_N05     5
#define CSP_N06     6
#define CSP_N07     7
#define CSP_N08     8
#define CSP_N09     9
#define CSP_PAG    10
#define CSP_GET    11
#define CSP_PUT    12
#define CSP_RES    13
#define CSP_REW    14
#define CSP_RDC    15
#define CSP_WRI    16
#define CSP_WRE    17
#define CSP_WRR    18
#define CSP_WRC    19
#define CSP_WRS    20
#define CSP_WRX    21
#define CSP_RDB    22
#define CSP_WRB    23
#define CSP_RDR    24
#define CSP_RDH    25
#define CSP_RDY    26
#define CSP_EOL    27
#define CSP_EOT    28
#define CSP_RDD    29
#define CSP_WRD    30
#define CSP_CLK    31
#define CSP_WLN    32
#define CSP_RLN    33
#define CSP_RDI    34
#define CSP_EOF    35
#define CSP_ELN    36
#define CSP_RDS    37
#define CSP_TRP    38
#define CSP_XIT    39
#define CSP_FDF    40
#define CSP_SIO    41
#define CSP_EIO    42
#define CSP_MSG    43
#define CSP_SKP    44
#define CSP_LIM    45
#define CSP_TRA    46
#define CSP_WRP    47
#define CSP_CLS    48
#define CSP_DAT    49
#define CSP_TIM    50
#define CSP_FLR    51
#define CSP_TRC    52
#define CSP_RND    53


static funtab ft [] =

{
   { "CTR", CSP_CTR, NULL,     0, 0, 0, ' ' },
   { "N01", CSP_N01, NULL,     0, 0, 0, ' ' },
   { "N02", CSP_N02, NULL,     0, 0, 0, ' ' },
   { "N03", CSP_N03, NULL,     0, 0, 0, ' ' },
   { "N04", CSP_N04, NULL,     0, 0, 0, ' ' },
   { "N05", CSP_N05, NULL,     0, 0, 0, ' ' },
   { "N06", CSP_N06, NULL,     0, 0, 0, ' ' },
   { "N07", CSP_N07, NULL,     0, 0, 0, ' ' },
   { "N08", CSP_N08, NULL,     0, 0, 0, ' ' },
   { "N09", CSP_N09, NULL,     0, 0, 0, ' ' },
   { "PAG", CSP_PAG, NULL,     0, 0, 0, ' ' },
   { "GET", CSP_GET, cspf_get, 1, 0, 0, ' ' },
   { "PUT", CSP_PUT, cspf_put, 1, 0, 0, ' ' },
   { "RES", CSP_RES, cspf_res, 1, 0, 0, ' ' },
   { "REW", CSP_REW, cspf_rew, 1, 0, 0, ' ' },
   { "RDC", CSP_RDC, cspf_rdc, 2, 1, 0, ' ' },
   { "WRI", CSP_WRI, cspf_wri, 3, 2, 0, ' ' },
   { "WRE", CSP_WRE, (cspfunc *) cspf_wre, -3, 3, 0, ' ' },
   { "WRR", CSP_WRR, (cspfunc *) cspf_wrr, -1, 4, 0, ' ' },
   { "WRC", CSP_WRC, cspf_wrc, 3, 2, 0, ' ' },
   { "WRS", CSP_WRS, cspf_wrs, 4, 3, 0, ' ' },
   { "WRX", CSP_WRX, cspf_wrx, 4, 3, 0, ' ' },
   { "RDB", CSP_RDB, cspf_rdb, 2, 1, 0, ' ' },
   { "WRB", CSP_WRB, cspf_wrb, 3, 2, 0, ' ' },
   { "RDR", CSP_RDR, cspf_rdr, 2, 1, 0, ' ' },
   { "RDH", CSP_RDH, cspf_rdh, 2, 1, 0, ' ' },
   { "RDY", CSP_RDY, NULL,     0, 0, 0, ' ' },
   { "EOL", CSP_EOL, cspf_eol, 1, -1, 0, ' ' },
   { "EOT", CSP_EOT, cspf_eot, 1, -1, 0, ' ' },
   { "RDD", CSP_RDD, NULL,     0, 0, 0, ' ' },
   { "WRD", CSP_WRD, cspf_wrd, 3, 2, 0, ' ' },
   { "CLK", CSP_CLK, cspf_clk, 1, 0, 0, ' ' },
   { "WLN", CSP_WLN, cspf_wln, 1, 0, 0, ' ' },
   { "RLN", CSP_RLN, cspf_rln, 1, 0, 0, ' ' },
   { "RDI", CSP_RDI, cspf_rdi, 2, 1, 0, ' ' },
   { "EOF", CSP_EOF, cspf_eof, 1, -1, 0, ' ' },
   { "ELN", CSP_ELN, cspf_eln, 1, -1, 0, ' ' },
   { "RDS", CSP_RDS, cspf_rds, 3, 2, 0, ' ' },
   { "TRP", CSP_TRP, NULL,     0, 0, 0, ' ' },
   { "XIT", CSP_XIT, cspf_xit, 1, 0, 0, ' ' },
   { "FDF", CSP_FDF, cspf_fdf, 3, 2, 0, ' ' },
   { "SIO", CSP_SIO, cspf_sio, 1, 0, 0, ' ' },
   { "EIO", CSP_EIO, cspf_eio, 1, 1, 0, ' ' },
   { "MSG", CSP_MSG, NULL,     0, 0, 0, ' ' },
   { "SKP", CSP_SKP, NULL,     0, 0, 0, ' ' },
   { "LIM", CSP_LIM, NULL,     0, 0, 0, ' ' },
   { "TRA", CSP_TRA, NULL,     0, 0, 0, ' ' },
   { "WRP", CSP_WRP, cspf_wrp, 3, 2, 0, ' ' },
   { "CLS", CSP_CLS, cspf_cls, 1, 0, 0, ' ' },
   { "DAT", CSP_DAT, cspf_dat, 0, 0, 0, ' ' },
   { "TIM", CSP_TIM, cspf_tim, 0, 0, 0, ' ' },
   { "FLR", CSP_FLR, (cspfunc *) cspf_flr, -2, 0, 0, ' ' },
   { "TRC", CSP_TRC, (cspfunc *) cspf_trc, -2, 0, 0, ' ' },
   { "RND", CSP_RND, (cspfunc *) cspf_rnd, -2, 0, 0, ' ' },
   {  NULL,      -1, NULL,     0, 0, 0, ' ' }
};







#define  BREMSE(x)                            \
                                              \
         fprintf (stderr, (x));               \
         if (gs -> stepanz > 1)               \
         {                                    \
            gs -> stepanz = 0;                \
            incr_ip = 0;                      \
         }

#define  BREMSE2(x,y)                         \
                                              \
         fprintf (stderr, (x), (y));          \
         if (gs -> stepanz > 1)               \
         {                                    \
            gs -> stepanz = 0;                \
            incr_ip = 0;                      \
         }




static char *cup_special (global_store *gs,
                          sc_code *pcode)

{
   int addr;
   int disp;
   double dwert;
   char *stackp;
   double *doublep;
   int first;
   int second;
   char *cmd;
   int system_rc;

   if (strcmp (pcode -> plabel, "$PASSYS ") == 0)
   {
      static char errbuf [80] = "";
      int x;
      int *intp;

      //**********************************************
      //   first argument = integer
      //   second argument = address
      //**********************************************

      addr = gs -> pcups + pcode -> x + 112;
      first = STOR_I (addr);
      second = STOR_I (addr + 4);

      switch (first)
      {
         case 1:
            x = store_alloc (gs, second);
            (gs -> sp) += 4;
            intp = ADDRSTACK (gs -> sp);
            *intp = x;
            break;

         case 10:
            x = STOR_I (second);
            (gs -> sp) += 4;
            intp = ADDRSTACK (gs -> sp);
            *intp = x;
            break;

         case 12:
            cmd = ADDRSTOR (second);
            system_rc = system (cmd);
            (gs -> sp) += 4;
            intp = ADDRSTACK (gs -> sp);
            *intp = system_rc;
            break;

         default:
            sprintf (errbuf,
                     "$PASSYS subfunction %d not yet implemented !!\n",
                     first);
            return errbuf;
            break;
      }

      return NULL;
   }
   else if (strcmp (pcode -> plabel, "DSQRT   ") == 0)
   {
      //**********************************************
      //   double argument at position pcode -> x
      //   compute sqrt and put result on stack
      //**********************************************

      disp = gs -> display [gs -> level];
      addr = disp + pcode -> x;
      addr = STOR_I (addr);
      dwert = STOR_R (addr);

      // printf ("Wert vor DSQRT = %15.7f\n", dwert);

      dwert = sqrt (dwert);

      (gs -> sp) += 4;

      stackp = ADDRSTACK (gs -> sp);
      (gs -> sp) += 4;
      doublep = (double *) stackp;

      *doublep = dwert;
      STACKTYPE (gs -> sp) = 'R';
   }
   else
   {
      return "+++ CUP not yet implemented !!\n";
   }

   return NULL;
}




static void int1 (global_store *gs)

/**********************************************************/
/*                                                        */
/*   Interpretieren eines einzelnen Befehls               */
/*                                                        */
/**********************************************************/

{
   sc_code *pcode;
   sc_code *pcodex;
   sc_code *pcodez;
   int opnum;
   int incr_ip = 1;

   char *stackp;
   char *storep;
   short *shortp;
   void **voidp;
   int *intp;
   int *intp2;
   unsigned int *uintp;
   unsigned int uwert;
   unsigned char *setp;
   unsigned char *setp1;
   unsigned char *setp2;
   unsigned char *cpu1;
   unsigned char *cpu2;
   int wert;
   int wert1;
   int wert2;
   char bool;
   char bool1;
   char bool2;
   char char1;
   int res;
   int addr;
   int addr1;
   int disp;
   int return_addr;
   int len;
   int len1;
   int len2;
   char setbuffer [SETLENMAX];

   funtab *pft;
   int parm1;
   int parm2;
   int parm3;
   int parm4;
   double parmd;
   double *parmdp;
   void *pres;
   int *pint;
   cspfunc *cfunc;
   cspfunc2 *cfunc2;
   cspfunc3 *cfunc3;

   char *charp;
   char *charp2;
   char *sourcep;

   double *doublep;
   double dwert;
   double dwert1;
   double dwert2;
   int sp2;

   int label1;
   int label2;
   int backs;

   char *errmsg;

   /************************************************/
   /*   Branch Table fuer XJP                      */
   /************************************************/

   int *btable;
   sc_code *pcode1;
   int defconst;
   int btarget;

   /************************************************/
   /*   fuer Prozeduraufrufe usw.                  */
   /************************************************/

   int newcups;
   cup_section *pcup;
   cup_section *pcupv;
   ent_section *pent;
   sc_code *pcode_ent;

   pcode = gs -> code0 + gs -> ip;
   opnum = gs -> ot [pcode -> op] . opnum;

   gs -> effadr = 0;

   switch (opnum)
   {
      case XXX_ABI:

         /************************************************/
         /*   absolute value                             */
         /************************************************/

         intp = ADDRSTACK (gs -> sp);
         if (*intp < 0)
            *intp = - (*intp);

         break;

      case XXX_ABR:

         /************************************************/
         /*   absolute value                             */
         /*   with doubles, the SP always points         */
         /*   to the middle of the 8 byte double         */
         /************************************************/

         doublep = ADDRSTACK ((gs -> sp) - 4);
         if (*doublep < 0)
            *doublep = - (*doublep);

         break;

      case XXX_ADA:

         /************************************************/
         /*   add 2 values (addresses) from top of stack */
         /************************************************/

         wert1 = STACK_I (gs -> sp);
         (gs -> sp) -= 4;
         intp = ADDRSTACK (gs -> sp);
         wert2 = *intp;
         *intp = wert2 + wert1;

         break;

      case XXX_ADI:

         /************************************************/
         /*   add 2 values from top of stack             */
         /************************************************/

         wert1 = STACK_I (gs -> sp);
         (gs -> sp) -= 4;
         intp = ADDRSTACK (gs -> sp);
         wert2 = *intp;
         *intp = wert2 + wert1;

         break;

      case XXX_ADR:

         /************************************************/
         /*   add 2 real values from top of stack        */
         /*   remove R flag from top stack element       */
         /*   reduce SP by 8 (because of real value)     */
         /************************************************/

         dwert1 = STACK_R ((gs -> sp) - 4);

         STACKTYPE (gs -> sp) = ' ';
         (gs -> sp) -= 8;

         doublep = ADDRSTACK ((gs -> sp) - 4);
         dwert2 = *doublep;
         *doublep = dwert2 + dwert1;

         break;

      case XXX_AND:

         /************************************************/
         /*   AND top 2 values from stack                */
         /*   different depending on type flag           */
         /************************************************/

         if (pcode -> t == 'B')
         {
            bool1 = STACK_C (gs -> sp);
            (gs -> sp) -= 4;
            intp = ADDRSTACK (gs -> sp);
            charp = (char *) intp;
            bool2 = *charp;
            *intp = bool1 && bool2;
         }
         else if (pcode -> t == 'I')
         {
            wert1 = STACK_I (gs -> sp);
            (gs -> sp) -= 4;
            intp = ADDRSTACK (gs -> sp);
            wert2 = *intp;
            *intp = wert1 & wert2;
         }
         else
         {
            BREMSE ("+++ default case !!\n");
         }

         break;

      case XXX_ASE:

         /************************************************/
         /*   add set element                            */
         /*   len1 = length of set                       */
         /*   if len1 < 0:                               */
         /*   different sequence of parameter on stack   */
         /************************************************/

         len1 = pcode -> q;

         if (len1 < 0)
         {
            len1 = - len1;

            wert1 = STACK_I (gs -> sp - 4);
            addr = STACK_I (gs -> sp);
            intp = ADDRSTACK (gs -> sp - 4);
            *intp = addr;
         }
         else
         {
            wert1 = STACK_I (gs -> sp);
            addr = STACK_I (gs -> sp - 4);
         }

         (gs -> sp) -= 4;

         setp = ADDRSTOR (SET_ADDR (addr));

         if (wert1 < len1 * 8)
         {
            int bytenr = wert1 / 8;
            int byteoffs = wert1 % 8;
            unsigned int byte;

            byte = 0x80;
            byte = byte >> byteoffs;
            setp [bytenr] |= byte;
         }

         break;

      case XXX_BGN:
         break;

      case XXX_CHK:

         /************************************************/
         /*   get value from top of stack                */
         /*   show runtime error, if out of bounds       */
         /************************************************/

         intp = ADDRSTACK (gs -> sp);
         wert1 = *intp;

         /************************************************/
         /*   check value                                */
         /************************************************/

         switch (pcode -> t)
         {
            case 'I':
               if (wert1 < pcode -> p || wert1 > pcode -> q)
                  runtime_error (gs, RANGEERR, NULL);
               break;

            case 'J':
               if (wert1 < pcode -> p || wert1 > pcode -> q)
                  runtime_error (gs, RANGEERR, NULL);
               break;

            default:
               BREMSE ("+++ default case !!\n");
               break;
         }

         break;

      case XXX_CHR:

         /************************************************/
         /*   get value from top of stack                */
         /*   force to character                         */
         /************************************************/

         uintp = ADDRSTACK (gs -> sp);

         uwert = *uintp;
         uwert &= 0xff;
         *uintp = uwert;

         break;

      case XXX_CSP:

         /************************************************/
         /*   call csp = standard procedure              */
         /************************************************/

         pft = ft + pcode -> q;
         if (pft -> cspnum != pcode -> q)
         {
            fprintf (stderr, "+++ Fatal Error in CSP-Table, "
                             "terminating\n");
            exit (1000);
         }

         stackp = ADDRSTACK (gs -> sp);

         if (pft -> func == NULL)
         {
            BREMSE ("+++ CSP not implemented !!\n");
         }
         else
         {
            switch (pft -> parmcnt)
            {
               case 0:
                  cfunc = pft -> func;
                  pres = (*cfunc) (gs, 0, 0, 0, 0);
                  break;
               case 1:
                  cfunc = pft -> func;
                  pint = (int *) stackp;
                  parm1 = *pint;
                  pres = (*cfunc) (gs, parm1, 0, 0, 0);
                  break;
               case 2:
                  cfunc = pft -> func;
                  pint = (int *) stackp;
                  parm2 = *pint;
                  pint --;
                  parm1 = *pint;
                  pres = (*cfunc) (gs, parm1, parm2, 0, 0);
                  break;
               case 3:
                  cfunc = pft -> func;
                  pint = (int *) stackp;
                  parm3 = *pint;
                  pint --;
                  parm2 = *pint;
                  pint --;
                  parm1 = *pint;
                  pres = (*cfunc) (gs, parm1, parm2, parm3, 0);
                  break;
               case 4:
                  cfunc = pft -> func;
                  pint = (int *) stackp;
                  parm4 = *pint;
                  pint --;
                  parm3 = *pint;
                  pint --;
                  parm2 = *pint;
                  pint --;
                  parm1 = *pint;
                  pres = (*cfunc) (gs, parm1, parm2, parm3, parm4);
                  break;

               //***************************************
               //*  special cases
               //*  -1 = WRR
               //*  -2 = cfunc3-Pointer (FLR, TRC, RND)
               //*  -3 = WRE
               //***************************************

               case -1:
                  cfunc2 = (cspfunc2 *) (pft -> func);
                  pint = (int *) stackp;
                  parm4 = *pint;
                  pint --;
                  parm3 = *pint;
                  pint -= 2;
                  parmd = *((double *) pint);
                  pint --;
                  parm1 = *pint;
                  pres = (*cfunc2) (gs, parm1, parmd, parm3, parm4);
                  break;

               case -2:
                  cfunc3 = (cspfunc3 *) (pft -> func);
                  pres = (*cfunc3) (gs, NULL);
                  break;

               case -3:
                  cfunc2 = (cspfunc2 *) (pft -> func);
                  pint = (int *) stackp;
                  parm3 = *pint;
                  pint -= 2;
                  parmd = *((double *) pint);
                  pint --;
                  parm1 = *pint;
                  pres = (*cfunc2) (gs, parm1, parmd, parm3, parm4);
                  break;
            }
         }

         (gs -> sp) -= 4 * pft -> numpops;

         break;

      case XXX_CST:
         BREMSE ("+++ not implemented !!\n");
         break;

      case XXX_CUP:

         if (pcode -> q == 0)
            runtime_error (gs, BADBRANCH, pcode -> poper);

         /************************************************/
         /*   special CUP call                           */
         /*   to be implemented by interpreter           */
         /************************************************/

         if (pcode -> q < 0)
         {
            errmsg = cup_special (gs, pcode);

            if (errmsg != NULL)
            {
               BREMSE (errmsg);
            }

            break;
         }

         /************************************************/
         /*   normal CUP call                            */
         /************************************************/

         pcode_ent = gs -> code0 + pcode -> q;

         /************************************************/
         /*   Build CUP Save Area at Pcode X position    */
         /************************************************/

         newcups = gs -> pcups + pcode -> x;

         pcup = ADDRSTOR (newcups);
         memcpy (pcup -> eyecatch, "CUPS", 4);

         /************************************************/
         /*   dynamic backchain                          */
         /*   new Save area is current pCups             */
         /************************************************/

         pcup -> backchain = gs -> pcups;
         pcup -> level_caller = gs -> level;

         /************************************************/
         /*   pruefen auf Stack/Heap Collision           */
         /*   new Save area is current pCups             */
         /************************************************/

         pent = (ent_section *) (pcode_ent -> psect);
         if ((gs -> maxstor = newcups + pent -> size) >
              gs -> hp)
         {
            runtime_error (gs, STACKCOLL, NULL);
         }

         pcup -> level_called = pcode_ent -> p;

         pcup -> olddisp = gs -> pcups;
         pcup -> newdisp = newcups;

         gs -> pcups = newcups;

         /************************************************/
         /*   static backchain                           */
         /************************************************/

         if (pcup -> level_called > pcup -> level_caller)
         {
            pcup -> back_static = pcup -> backchain;
         }
         else if (pcup -> level_called == 2)
         {
            pcup -> back_static = 0;
         }
         else
         {
            int bchain;

            bchain = pcup -> backchain;
            pcupv = ADDRSTOR (bchain);

            for (;;)
            {
               if (pcupv -> level_called < pcup -> level_called)
                  break;

               bchain = pcupv -> backchain;
               pcupv = ADDRSTOR (bchain);
            }

            pcup -> back_static = bchain;
         }

         /************************************************/
         /*   call addr and return addr                  */
         /************************************************/

         pcup -> calladdr = pcode -> q;
         pcup -> returnaddr = gs -> ip + 1;

         /************************************************/
         /*   set new display value                      */
         /************************************************/

         gs -> level = pcup -> level_called;
         gs -> display [gs -> level] = pcup -> newdisp;

         /************************************************/
         /*   trace output                               */
         /************************************************/

#if 0

         fprintf (stderr, "CUP: ------------------------"
                          "---------------------------\n");
         fprintf (stderr, "CUP: newcups = %d\n", newcups);
         fprintf (stderr, "CUP: newlevel = %d\n", gs -> level);

         for (i = 1; i <= gs -> level; i ++)
            fprintf (stderr, "CUP: Level = %d Display = %d\n",
                     i, gs -> display [i]);

         fprintf (stderr, "CUP: Backchain = %d\n",
                  pcup -> backchain);

         pcupv = pcup;

         for (;;)
         {
            fprintf (stderr, "CUP: &BackStatic = %p\n",
                     &(pcupv -> back_static));
            fprintf (stderr, "CUP: BackStatic = %d\n",
                     pcupv -> back_static);

            if (pcupv -> back_static == 0)
               break;

            pcupv = ADDRSTOR (pcupv -> back_static);
         }

#endif

         /************************************************/
         /*   branch to procedure entry                  */
         /************************************************/

         gs -> ip = pcode -> q;
         incr_ip = 0;

         break;

      case XXX_DBG:
         gs -> stepanz = 0;
         break;

      case XXX_DEC:

         /************************************************/
         /*   decrement value at sp position             */
         /************************************************/

         intp = ADDRSTACK (gs -> sp);
         (*intp) -= pcode -> q;
         break;

      case XXX_DEF:
         break;

      case XXX_DFC:
         break;

      case XXX_DIF:

         /************************************************/
         /*   set difference                             */
         /************************************************/
         /*   korrektur am 18.12.2016:                   */
         /*   der groessere von den beiden operanden     */
         /*   wird der ziel-set und bleibt uebrig;       */
         /*   der andere wird gepoppt                    */
         /************************************************/

         addr = STACK_I (gs -> sp);
         setp1 = ADDRSTOR (SET_ADDR (addr));
         len1 = SET_LEN (addr);

         (gs -> sp) -= 4;

         addr = STACK_I (gs -> sp);
         setp2 = ADDRSTOR (SET_ADDR (addr));
         len2 = SET_LEN (addr);

         if (len2 >= len1)
         {
            len = len1;

            for (cpu1 = setp1, cpu2 = setp2;
                 len > 0;
                 cpu1 += 1, cpu2 += 1, len -= 1)
            {
               *cpu2 -= (*cpu2 & *cpu1);
            }
         }
         else
         {
            len = len2;

            for (cpu1 = setp1, cpu2 = setp2;
                 len > 0;
                 cpu1 += 1, cpu2 += 1, len -= 1)
            {
               *cpu1 -= (*cpu1 & *cpu2);
            }

            stackp = ADDRSTACK (gs -> sp);
            intp = (int *) stackp;
            intp [0] = intp [1];
         }

         break;

      case XXX_DVI:

         /************************************************/
         /*   Divid. der oberen beiden Stack Elemente    */
         /************************************************/

         wert1 = STACK_I (gs -> sp);
         (gs -> sp) -= 4;
         intp = ADDRSTACK (gs -> sp);
         wert2 = *intp;

         *intp = wert2 / wert1;

         break;

      case XXX_DVR:

         /************************************************/
         /*   Divid. der oberen beiden Stack Elemente    */
         /*   remove R flag from top stack element       */
         /*   reduce SP by 8 (because of real value)     */
         /************************************************/

         dwert1 = STACK_R ((gs -> sp) - 4);

         STACKTYPE (gs -> sp) = ' ';
         (gs -> sp) -= 8;

         doublep = ADDRSTACK ((gs -> sp) - 4);
         dwert2 = *doublep;
         *doublep = dwert2 / dwert1;

         break;

      case XXX_END:
         BREMSE ("+++ not implemented !!\n");
         break;

      case XXX_ENT:

         /**********************************************************/
         /*   was muss passieren?                                  */
         /*   beim Rufer wird angegeben, wo im eigenen Bereich     */
         /*   der Bereich des neuen Blocks beginnen soll (x),      */
         /*   diese Zahl steht bei CUP.                            */
         /*   Wir koennen ja den SP entsprechen setzen bei CUP.    */
         /*   Demnach kann man auch das neue DISPLAY davon         */
         /*   ableiten; allerdings muss sichergestellt sein,       */
         /*   dass der alte DISPLAY-Vektor bei Rueckkehr wieder    */
         /*   hergestellt werden kann.                             */
         /**********************************************************/

         gs -> level = pcode -> p;

         break;

      case XXX_EQU:

         /************************************************/
         /*   compare values                             */
         /************************************************/

         if (pcode -> t == 'M')
         {
            /************************************************/
            /*   get 2 values from top of stack             */
            /************************************************/

            wert1 = STACK_I (gs -> sp);
            (gs -> sp) -= 4;
            wert2 = STACK_I (gs -> sp);

            res = memcmp (ADDRSTOR (wert2),
                          ADDRSTOR (wert1),
                          pcode -> q);
         }
         else if (pcode -> t == 'R')
         {
            /************************************************/
            /*   get 2 real values from top of stack        */
            /************************************************/

            dwert1 = STACK_R ((gs -> sp) - 4);

            STACKTYPE (gs -> sp) = ' ';
            (gs -> sp) -= 8;

            dwert2 = STACK_R ((gs -> sp) - 4);

            STACKTYPE (gs -> sp) = ' ';
            (gs -> sp) -= 4;

            STACKTYPE (gs -> sp) = 'B';

            if (dwert2 == dwert1)
               res = 0;
            else
               res = 1;
         }
         else if (pcode -> t == 'S')
         {
            /************************************************/
            /*   compare 2 set values                       */
            /************************************************/

            addr = STACK_I (gs -> sp);
            setp1 = ADDRSTOR (SET_ADDR (addr));
            len1 = SET_LEN (addr);

            STACKTYPE (gs -> sp) = ' ';
            (gs -> sp) -= 4;

            addr = STACK_I (gs -> sp);
            setp2 = ADDRSTOR (SET_ADDR (addr));
            len2 = SET_LEN (addr);

            STACKTYPE (gs -> sp) = 'B';

            if (len1 != len2)
               runtime_error (gs, SETLERROR, NULL);

            res = memcmp (setp2, setp1, len1);
         }
         else
         {
            /************************************************/
            /*   get 2 values from top of stack             */
            /************************************************/

            wert1 = STACK_I (gs -> sp);
            (gs -> sp) -= 4;
            wert2 = STACK_I (gs -> sp);

            res = wert2 - wert1;
         }

         /************************************************/
         /*   boolean value replaces stack position      */
         /************************************************/

         bool = (res == 0 ? 1 : 0);
         intp = ADDRSTACK (gs -> sp);
         *intp = bool;

         break;

      case XXX_FJP:

         /************************************************/
         /*   get value from top of stack                */
         /************************************************/

         stackp = ADDRSTACK (gs -> sp);
         intp = (int *) stackp;
         wert1 = *intp;

         /************************************************/
         /*   decrement stack pointer                    */
         /************************************************/

         (gs -> sp) -= 4;

         if (! wert1)
         {
            gs -> ip = pcode -> q;
            incr_ip = 0;
         }

         break;

      case XXX_FLO:

         /************************************************/
         /*   converts the value second on the stack     */
         /*   to float; first value has to be moved      */
         /************************************************/

         if (STACKTYPE (gs -> sp) == 'R')
         {
            dwert = STACK_R ((gs -> sp) - 4);

            STACKTYPE (gs -> sp) = ' ';
            (gs -> sp) += 4;

            STACKTYPE (gs -> sp) = 'R';

            stackp = ADDRSTACK ((gs -> sp) - 4);
            doublep = (double *) stackp;
            *doublep = dwert;

            sp2 = (gs -> sp) - 12;
            intp = ADDRSTACK (sp2);
         }
         else
         {
            stackp = ADDRSTACK (gs -> sp);
            intp = (int *) stackp;
            intp [1] = intp [0];
            (gs -> sp) += 4;

            sp2 = (gs -> sp) - 8;
            intp = ADDRSTACK (sp2);
         }

         dwert = *intp;

         doublep = (double *) intp;
         *doublep = dwert;

         STACKTYPE (sp2) = ' ';
         STACKTYPE (sp2 + 4) = 'R';

         break;

      case XXX_FLR:

         /************************************************/
         /*   floor function applied to the real         */
         /*   value on top of the stack                  */
         /************************************************/

         doublep = ADDRSTACK ((gs -> sp) - 4);
         *doublep = floor (*doublep);

         STACKTYPE (gs -> sp) = 'R';

         break;

      case XXX_FLT:

         /************************************************/
         /*   converts the value first on the stack      */
         /*   to float                                   */
         /************************************************/

         intp = ADDRSTACK (gs -> sp);
         dwert = *intp;

         doublep = (double *) intp;
         *doublep = dwert;

         STACKTYPE (gs -> sp) = ' ';
         (gs -> sp) += 4;

         STACKTYPE (gs -> sp) = 'R';

         break;

      case XXX_GEQ:

         /************************************************/
         /*   compare values                             */
         /************************************************/

         if (pcode -> t == 'M')
         {
            /************************************************/
            /*   get 2 values from top of stack             */
            /************************************************/

            wert1 = STACK_I (gs -> sp);
            (gs -> sp) -= 4;
            wert2 = STACK_I (gs -> sp);

            res = memcmp (ADDRSTOR (wert2),
                          ADDRSTOR (wert1),
                          pcode -> q);
         }
         else if (pcode -> t == 'R')
         {
            /************************************************/
            /*   get 2 real values from top of stack        */
            /************************************************/

            dwert1 = STACK_R ((gs -> sp) - 4);

            STACKTYPE (gs -> sp) = ' ';
            (gs -> sp) -= 8;

            dwert2 = STACK_R ((gs -> sp) - 4);

            STACKTYPE (gs -> sp) = ' ';
            (gs -> sp) -= 4;

            STACKTYPE (gs -> sp) = 'B';

            if (dwert2 >= dwert1)
               res = 1;
            else
               res = -1;
         }
         else
         {
            /************************************************/
            /*   get 2 values from top of stack             */
            /************************************************/

            wert1 = STACK_I (gs -> sp);
            (gs -> sp) -= 4;
            wert2 = STACK_I (gs -> sp);

            res = wert2 - wert1;
         }

         /************************************************/
         /*   boolean value replaces stack position      */
         /************************************************/

         bool = (res >= 0 ? 1 : 0);
         intp = ADDRSTACK (gs -> sp);
         *intp = bool;

         break;

      case XXX_GRT:

         /************************************************/
         /*   compare values                             */
         /************************************************/

         if (pcode -> t == 'M')
         {
            /************************************************/
            /*   get 2 values from top of stack             */
            /************************************************/

            wert1 = STACK_I (gs -> sp);
            (gs -> sp) -= 4;
            wert2 = STACK_I (gs -> sp);

            res = memcmp (ADDRSTOR (wert2),
                          ADDRSTOR (wert1),
                          pcode -> q);
         }
         else if (pcode -> t == 'R')
         {
            /************************************************/
            /*   get 2 real values from top of stack        */
            /************************************************/

            dwert1 = STACK_R ((gs -> sp) - 4);

            STACKTYPE (gs -> sp) = ' ';
            (gs -> sp) -= 8;

            dwert2 = STACK_R ((gs -> sp) - 4);

            STACKTYPE (gs -> sp) = ' ';
            (gs -> sp) -= 4;

            STACKTYPE (gs -> sp) = 'B';

            if (dwert2 > dwert1)
               res = 1;
            else
               res = -1;
         }
         else
         {
            /************************************************/
            /*   get 2 values from top of stack             */
            /************************************************/

            wert1 = STACK_I (gs -> sp);
            (gs -> sp) -= 4;
            wert2 = STACK_I (gs -> sp);

            res = wert2 - wert1;
         }

         /************************************************/
         /*   boolean value replaces stack position      */
         /************************************************/

         bool = (res > 0 ? 1 : 0);
         intp = ADDRSTACK (gs -> sp);
         *intp = bool;

         break;

      case XXX_INC:

         /************************************************/
         /*   increment value at sp position             */
         /************************************************/

         intp = ADDRSTACK (gs -> sp);
         (*intp) += pcode -> q;
         break;

      case XXX_IND:

         /************************************************/
         /*   indirect access by addr at top of stack    */
         /************************************************/

         stackp = ADDRSTACK (gs -> sp);
         intp = (int *) stackp;
         sourcep = ADDRSTOR (*intp);

         /************************************************/
         /*   incr addr                                  */
         /************************************************/

         sourcep += pcode -> q;

         /************************************************/
         /*   load constant at stack position            */
         /************************************************/

         switch (pcode -> t)
         {
            case 'A':
               voidp = (void **) stackp;
               *voidp = *((void **) sourcep);
               break;

            case 'B':
            case 'C':
               intp = (int *) stackp;
               *intp = 0;
               charp = (char *) stackp;
               *charp = *((char *) sourcep);
               break;

            case 'I':
               intp = (int *) stackp;
               *intp = *((int *) sourcep);
               break;

            case 'H':

               /************************************************/
               /*   expand to long format when writing         */
               /*   to stack                                   */
               /************************************************/

               intp = (int *) stackp;
               wert = *((short *) sourcep);
               *intp = wert;
               break;

            case 'R':
               doublep = (double *) stackp;
               *doublep = *((double *) sourcep);
               (gs -> sp) += 4;
               STACKTYPE (gs -> sp) = 'R';
               break;

            default:
               BREMSE ("+++ default case !!\n");
               break;
         }

         break;

      case XXX_INN:

         wert1 = STACK_I (gs -> sp - 4);

         addr = STACK_I (gs -> sp);
         setp = ADDRSTOR (SET_ADDR (addr));
         len1 = SET_LEN (addr);

#if 0

         fprintf (stderr, "INN: wert1 = %d\n", wert1);
         fprintf (stderr, "INN: set/addr = %d\n", SET_ADDR (addr));
         fprintf (stderr, "INN: set/len = %d\n", SET_LEN (addr));
         fprintf (stderr, "INN: len1 = %d\n", len1);

#endif

         bool = 0;

         if (wert1 < len1 * 8)
         {
            int bytenr = wert1 / 8;
            int byteoffs = wert1 % 8;
            unsigned int byte;

            byte = setp [bytenr];
            byte = byte << byteoffs;
            byte &= 0x80;

            bool = (byte != 0);
         }

#if 0

         fprintf (stderr, "INN: bool = %d\n", bool);

#endif

         (gs -> sp) -= 4;
         intp = ADDRSTACK (gs -> sp);
         charp = (char *) intp;
         *intp = 0;
         *charp = bool;

         break;

      case XXX_INT:

         /************************************************/
         /*   set intersection                           */
         /************************************************/
         /*   korrektur am 18.12.2016:                   */
         /*   der groessere von den beiden operanden     */
         /*   wird der ziel-set und bleibt uebrig;       */
         /*   der andere wird gepoppt                    */
         /************************************************/

         addr = STACK_I (gs -> sp);
         setp1 = ADDRSTOR (SET_ADDR (addr));
         len1 = SET_LEN (addr);

         (gs -> sp) -= 4;

         addr = STACK_I (gs -> sp);
         setp2 = ADDRSTOR (SET_ADDR (addr));
         len2 = SET_LEN (addr);

         if (len2 >= len1)
         {
            len = len1;

            for (cpu1 = setp1, cpu2 = setp2;
                 len > 0;
                 cpu1 += 1, cpu2 += 1, len -= 1)
            {
               *cpu2 &= *cpu1;
            }
         }
         else
         {
            len = len2;

            for (cpu1 = setp1, cpu2 = setp2;
                 len > 0;
                 cpu1 += 1, cpu2 += 1, len -= 1)
            {
               *cpu1 &= *cpu2;
            }

            stackp = ADDRSTACK (gs -> sp);
            intp = (int *) stackp;
            intp [0] = intp [1];
         }

         break;

      case XXX_IOR:

         /************************************************/
         /*   OR top 2 values from stack                 */
         /*   different depending on type flag           */
         /************************************************/

         if (pcode -> t == 'B')
         {
            bool1 = STACK_C (gs -> sp);
            (gs -> sp) -= 4;
            intp = ADDRSTACK (gs -> sp);
            charp = (char *) intp;
            bool2 = *charp;
            *intp = bool1 || bool2;
         }
         else if (pcode -> t == 'I')
         {
            wert1 = STACK_I (gs -> sp);
            (gs -> sp) -= 4;
            intp = ADDRSTACK (gs -> sp);
            wert2 = *intp;
            *intp = wert1 | wert2;
         }
         else
         {
            BREMSE ("+++ default case !!\n");
         }

         break;

      case XXX_IXA:

         /************************************************/
         /*   get value from top of stack                */
         /************************************************/

         stackp = ADDRSTACK (gs -> sp);
         intp = (int *) stackp;
         wert = *intp;

         /************************************************/
         /*   decrement stack pointer                    */
         /************************************************/

         (gs -> sp) -= 4;

         /************************************************/
         /*   multiply value by element size =           */
         /*   index arithmetic                           */
         /************************************************/

         wert *= pcode -> q;

         /************************************************/
         /*   increment address on top of stack          */
         /************************************************/

         stackp = ADDRSTACK (gs -> sp);
         intp = (int *) stackp;
         *intp += wert;
         break;

      case XXX_LAB:
         break;

      case XXX_LCA:

         /************************************************/
         /*   increment stack pointer                    */
         /************************************************/

         (gs -> sp) += 4;
         stackp = ADDRSTACK (gs -> sp);

         /************************************************/
         /*   load address from instruction              */
         /*   the same, when set involved                */
         /*   length is added on following SLD           */
         /*   (must be the same as set const length)     */
         /************************************************/

         switch (pcode -> t)
         {
            case 'S':
               addr = pcode -> q;
               break;
            default:
               addr = pcode -> q;
               break;
         }

         intp = (int *) stackp;
         *intp = addr;

         STACKTYPE (gs -> sp) = 'A';

         break;

      case XXX_LDA:

         /************************************************/
         /*   increment stack pointer                    */
         /************************************************/

         (gs -> sp) += 4;
         stackp = ADDRSTACK (gs -> sp);

         /************************************************/
         /*   load level / address at stack position     */
         /************************************************/

         disp = gs -> display [pcode -> p];
         addr = disp + pcode -> q;
         gs -> effadr = addr;

         intp = (int *) stackp;
         *intp = addr;

         STACKTYPE (gs -> sp) = 'A';

         break;

      case XXX_LDC:

         /************************************************/
         /*   increment stack pointer                    */
         /************************************************/

         (gs -> sp) += 4;
         stackp = ADDRSTACK (gs -> sp);

         /************************************************/
         /*   load constant at stack position            */
         /*   expand H constant to I format - 12.2016    */
         /************************************************/

         switch (pcode -> t)
         {
            case 'B':
            case 'C':
               intp = (int *) stackp;
               *intp = 0;
               *stackp = pcode -> q;
               STACKTYPE (gs -> sp) = pcode -> t;
               break;
            case 'N':
               intp = (int *) stackp;
               *intp = -1;
               STACKTYPE (gs -> sp) = pcode -> t;
               break;
            case 'H':
            case 'I':
               intp = (int *) stackp;
               *intp = pcode -> q;
               STACKTYPE (gs -> sp) = pcode -> t;
               break;

            case 'R':

               /************************************************/
               /*   increment stack again to hold 8 bytes      */
               /*   for double value                           */
               /************************************************/

               doublep = (double *) stackp;
               sourcep = ADDRSTOR (pcode -> q);
               *doublep = *((double *) sourcep);

               (gs -> sp) += 4;
               STACKTYPE (gs -> sp) = pcode -> t;

               break;

            default:
               BREMSE ("+++ default case !!\n");
               break;
         }

         break;

      case XXX_LEQ:

         /************************************************/
         /*   compare values                             */
         /************************************************/

         if (pcode -> t == 'M')
         {
            /************************************************/
            /*   get 2 values from top of stack             */
            /************************************************/

            wert1 = STACK_I (gs -> sp);
            (gs -> sp) -= 4;
            wert2 = STACK_I (gs -> sp);

            res = memcmp (ADDRSTOR (wert2),
                          ADDRSTOR (wert1),
                          pcode -> q);
         }
         else if (pcode -> t == 'R')
         {
            /************************************************/
            /*   get 2 real values from top of stack        */
            /************************************************/

            dwert1 = STACK_R ((gs -> sp) - 4);

            STACKTYPE (gs -> sp) = ' ';
            (gs -> sp) -= 8;

            dwert2 = STACK_R ((gs -> sp) - 4);

            STACKTYPE (gs -> sp) = ' ';
            (gs -> sp) -= 4;

            STACKTYPE (gs -> sp) = 'B';

            if (dwert2 <= dwert1)
               res = -1;
            else
               res = 1;
         }
         else
         {
            /************************************************/
            /*   get 2 values from top of stack             */
            /************************************************/

            wert1 = STACK_I (gs -> sp);
            (gs -> sp) -= 4;
            wert2 = STACK_I (gs -> sp);

            res = wert2 - wert1;
         }

         /************************************************/
         /*   boolean value replaces stack position      */
         /************************************************/

         bool = (res <= 0 ? 1 : 0);
         intp = ADDRSTACK (gs -> sp);
         *intp = bool;

         break;

      case XXX_LES:

         /************************************************/
         /*   compare values                             */
         /************************************************/

         if (pcode -> t == 'M')
         {
            /************************************************/
            /*   get 2 values from top of stack             */
            /************************************************/

            wert1 = STACK_I (gs -> sp);
            (gs -> sp) -= 4;
            wert2 = STACK_I (gs -> sp);

            res = memcmp (ADDRSTOR (wert2),
                          ADDRSTOR (wert1),
                          pcode -> q);
         }
         else if (pcode -> t == 'R')
         {
            /************************************************/
            /*   get 2 real values from top of stack        */
            /************************************************/

            dwert1 = STACK_R ((gs -> sp) - 4);

            STACKTYPE (gs -> sp) = ' ';
            (gs -> sp) -= 8;

            dwert2 = STACK_R ((gs -> sp) - 4);

            STACKTYPE (gs -> sp) = ' ';
            (gs -> sp) -= 4;

            STACKTYPE (gs -> sp) = 'B';

            if (dwert2 < dwert1)
               res = -1;
            else
               res = 1;
         }
         else
         {
            /************************************************/
            /*   get 2 values from top of stack             */
            /************************************************/

            wert1 = STACK_I (gs -> sp);
            (gs -> sp) -= 4;
            wert2 = STACK_I (gs -> sp);

            res = wert2 - wert1;
         }

         /************************************************/
         /*   boolean value replaces stack position      */
         /************************************************/

         bool = (res < 0 ? 1 : 0);
         intp = ADDRSTACK (gs -> sp);
         *intp = bool;

         break;

      case XXX_LOC:

         gs -> lineofcode = pcode -> q;
         break;

      case XXX_LOD:

         /************************************************/
         /*   get value from level / address             */
         /************************************************/

         disp = gs -> display [pcode -> p];
         addr = disp + pcode -> q;
         gs -> effadr = addr;

         storep = ADDRSTOR (addr);

         switch (pcode -> t)
         {
            case 'B':
            case 'C':
               wert = *storep;
               break;

            case 'H':

               /************************************************/
               /*   expand to long format when writing         */
               /*   to stack                                   */
               /************************************************/

               shortp = (short *) storep;
               wert = *shortp;
               break;

            case 'A':
            case 'I':
               intp = (int *) storep;
               wert = *intp;
               break;

            case 'R':
               doublep = (double *) storep;
               dwert = *doublep;
               break;

            default:
               BREMSE ("+++ default case !!\n");
               break;
         }

         /************************************************/
         /*   increment stack pointer                    */
         /************************************************/

         (gs -> sp) += 4;
         stackp = ADDRSTACK (gs -> sp);

         /************************************************/
         /*   load value at stack position               */
         /************************************************/

         switch (pcode -> t)
         {
            case 'B':
            case 'C':
               intp = (int *) stackp;
               *intp = 0;
               *stackp = wert;
               STACKTYPE (gs -> sp) = pcode -> t;
               break;
            case 'A':
            case 'I':
            case 'H':
               intp = (int *) stackp;
               *intp = wert;
               STACKTYPE (gs -> sp) = pcode -> t;
               break;

            case 'R':

               /************************************************/
               /*   increment stack again to hold 8 bytes      */
               /*   for double value                           */
               /************************************************/

               doublep = (double *) stackp;
               *doublep = dwert;

               (gs -> sp) += 4;
               STACKTYPE (gs -> sp) = 'R';

               break;

            default:
               BREMSE ("+++ default case !!\n");
               break;
         }
         break;

      case XXX_MOD:

         /************************************************/
         /*   <SP - 4> mod <SP>                          */
         /************************************************/

         wert1 = STACK_I (gs -> sp);
         (gs -> sp) -= 4;
         intp = ADDRSTACK (gs -> sp);
         wert2 = *intp;

         *intp = wert2 % wert1;

         break;

      case XXX_MCP:

         /************************************************/
         /*   memcpy inline                              */
         /*   get length from SP                         */
         /*   get source address from SP - 1             */
         /*   get target address from SP - 2             */
         /*   pop all three items                        */
         /************************************************/

         wert1 = STACK_I (gs -> sp);
         (gs -> sp) -= 4;

         intp = ADDRSTACK (gs -> sp);
         charp = ADDRSTOR (*intp);
         (gs -> sp) -= 4;

         intp = ADDRSTACK (gs -> sp);
         charp2 = ADDRSTOR (*intp);
         (gs -> sp) -= 4;

         if (wert1 > 0)
         {
            memcpy (charp2, charp, wert1);
         }

         break;

      case XXX_MFI:

         if (pcode -> q < 0)
         {
            /************************************************/
            /*   get char pattern from SP                   */
            /*   ignore SP - 1                              */
            /*   get target address from SP - 2             */
            /*   only pop char pattern                      */
            /*   (because two addresses on stack are        */
            /*   use for subsequent MOV)                    */
            /************************************************/

            char1 = STACK_C (gs -> sp);

            (gs -> sp) -= 8;
            intp = ADDRSTACK (gs -> sp);
            charp = ADDRSTOR (*intp);
            (gs -> sp) += 4;

            memset (charp, char1, - (pcode -> q));
         }
         else
         {
            /************************************************/
            /*   get char pattern from SP                   */
            /*   get target address from SP - 1             */
            /*   pop two items                              */
            /************************************************/

            char1 = STACK_C (gs -> sp);

            (gs -> sp) -= 4;
            intp = ADDRSTACK (gs -> sp);
            charp = ADDRSTOR (*intp);
            (gs -> sp) -= 4;

            memset (charp, char1, pcode -> q);
         }

         break;

      case XXX_MOV:

         /************************************************/
         /*   indirect access by addr at top of stack    */
         /************************************************/

         intp = ADDRSTACK (gs -> sp);
         charp = ADDRSTOR (*intp);
         (gs -> sp) -= 4;

         /************************************************/
         /*   indirect access by addr at top of stack    */
         /************************************************/

         intp = ADDRSTACK (gs -> sp);
         charp2 = ADDRSTOR (*intp);
         (gs -> sp) -= 4;

         /************************************************/
         /*   move bytes                                 */
         /************************************************/

         if (pcode -> q < 0)
         {
            memcpy (charp, charp2, - (pcode -> q));
         }
         else
         {
            memcpy (charp2, charp, pcode -> q);
         }

         break;

      case XXX_MPI:

         /************************************************/
         /*   multiply values at top of stack            */
         /************************************************/

         wert1 = STACK_I (gs -> sp);
         (gs -> sp) -= 4;
         intp = ADDRSTACK (gs -> sp);
         wert2 = *intp;
         *intp = wert2 * wert1;

         break;

      case XXX_MPR:


         /************************************************/
         /*   multiply real values at top of stack       */
         /*   remove R flag from top stack element       */
         /*   reduce SP by 8 (because of real value)     */
         /************************************************/

         dwert1 = STACK_R ((gs -> sp) - 4);

         STACKTYPE (gs -> sp) = ' ';
         (gs -> sp) -= 8;

         doublep = ADDRSTACK ((gs -> sp) - 4);
         dwert2 = *doublep;
         *doublep = dwert2 * dwert1;

         break;

      case XXX_MSE:

         /************************************************/
         /*   memset inline                              */
         /*   get length from SP                         */
         /*   get pattern from SP - 1                    */
         /*   get target address from SP - 2             */
         /*   pop all three items                        */
         /************************************************/

         wert1 = STACK_I (gs -> sp);
         (gs -> sp) -= 4;

         char1 = STACK_C (gs -> sp);
         (gs -> sp) -= 4;

         intp = ADDRSTACK (gs -> sp);
         charp = ADDRSTOR (*intp);
         (gs -> sp) -= 4;

         if (wert1 > 0)
         {
            memset (charp, char1, wert1);
         }

         break;

      case XXX_MST:

         /************************************************/
         /*   do nothing, the relevant actions           */
         /*   are carried out by CUP                     */
         /************************************************/

         break;

      case XXX_MZE:

         /************************************************/
         /*   get target address from SP                 */
         /*   pop one item                               */
         /************************************************/

         intp = ADDRSTACK (gs -> sp);
         charp = ADDRSTOR (*intp);
         (gs -> sp) -= 4;

         memset (charp, 0x00, pcode -> q);

         break;

      case XXX_NEQ:

         /************************************************/
         /*   compare values                             */
         /************************************************/

         if (pcode -> t == 'M')
         {
            /************************************************/
            /*   get 2 values from top of stack             */
            /************************************************/

            wert1 = STACK_I (gs -> sp);
            (gs -> sp) -= 4;
            wert2 = STACK_I (gs -> sp);

            res = memcmp (ADDRSTOR (wert2),
                          ADDRSTOR (wert1),
                          pcode -> q);
         }
         else if (pcode -> t == 'S')
         {
            /************************************************/
            /*   compare 2 set values                       */
            /************************************************/

            addr = STACK_I (gs -> sp);
            setp1 = ADDRSTOR (SET_ADDR (addr));
            len1 = SET_LEN (addr);

            STACKTYPE (gs -> sp) = ' ';
            (gs -> sp) -= 4;

            addr = STACK_I (gs -> sp);
            setp2 = ADDRSTOR (SET_ADDR (addr));
            len2 = SET_LEN (addr);

            STACKTYPE (gs -> sp) = 'B';

            if (len1 != len2)
               runtime_error (gs, SETLERROR, NULL);

            res = memcmp (setp2, setp1, len1);
         }
         else if (pcode -> t == 'R')
         {
            /************************************************/
            /*   get 2 real values from top of stack        */
            /************************************************/

            dwert1 = STACK_R ((gs -> sp) - 4);

            STACKTYPE (gs -> sp) = ' ';
            (gs -> sp) -= 8;

            dwert2 = STACK_R ((gs -> sp) - 4);

            STACKTYPE (gs -> sp) = ' ';
            (gs -> sp) -= 4;

            STACKTYPE (gs -> sp) = 'B';

            if (dwert2 != dwert1)
               res = 1;
            else
               res = 0;
         }
         else
         {
            /************************************************/
            /*   get 2 values from top of stack             */
            /************************************************/

            wert1 = STACK_I (gs -> sp);
            (gs -> sp) -= 4;
            wert2 = STACK_I (gs -> sp);

            res = wert2 - wert1;
         }

         /************************************************/
         /*   boolean value replaces stack position      */
         /************************************************/

         bool = (res != 0 ? 1 : 0);
         intp = ADDRSTACK (gs -> sp);
         *intp = bool;

         break;

      case XXX_NEW:

         addr = STACK_I (gs -> sp);
         storep = ADDRSTOR (addr);

         wert1 = pcode -> p;
         wert2 = pcode -> q;

         gs -> hp -= wert1;
         gs -> hp -= (gs -> hp % wert2);

         if (gs -> maxstor > gs -> hp)
         {
            runtime_error (gs, STACKCOLL, NULL);
         }

         intp = (int *) storep;
         *intp = (gs -> hp);
         (gs -> sp) -= 4;

         break;

      case XXX_NGI:

         /************************************************/
         /*   negative value                             */
         /************************************************/

         intp = ADDRSTACK (gs -> sp);
         *intp = - (*intp);

         break;

      case XXX_NGR:

         /************************************************/
         /*   negative double value                      */
         /************************************************/

         doublep = ADDRSTACK ((gs -> sp) - 4);
         *doublep = - (*doublep);

         break;

      case XXX_NOT:

         /************************************************/
         /*   get value from top of stack                */
         /*   different depending on type flag           */
         /************************************************/

         if (pcode -> t == 'B')
         {
            stackp = ADDRSTACK (gs -> sp);
            intp = (int *) stackp;
            *intp = ! (*intp);
         }
         else if (pcode -> t == 'I')
         {
            stackp = ADDRSTACK (gs -> sp);
            for (cpu1 = (unsigned char *) stackp;
                 cpu1 < (unsigned char *) stackp + sizeof (int);
                 cpu1 ++)
            {
               *cpu1 ^= 0xffu;
            }
         }
         else
         {
            BREMSE ("+++ default case !!\n");
         }

         break;

      case XXX_ODD:

         wert = STACK_I (gs -> sp);

         bool = ((wert & 1) != 0 ? 1 : 0);
         intp = ADDRSTACK (gs -> sp);
         *intp = bool;

         break;

      case XXX_ORD:

         break;

      case XXX_RET:

         /************************************************/
         /*   get return value from location 72          */
         /*   (fix position) and push it on the stack    */
         /************************************************/

         switch (pcode -> t)
         {
            case 'A':
               (gs -> sp) += 4;
               stackp = ADDRSTACK (gs -> sp);
               intp = (int *) stackp;

               return_addr = gs -> display [gs -> level] + 72;
               *intp = STOR_I (return_addr);

               break;

            case 'I':
               (gs -> sp) += 4;
               stackp = ADDRSTACK (gs -> sp);
               intp = (int *) stackp;

               return_addr = gs -> display [gs -> level] + 72;
               *intp = STOR_I (return_addr);

               break;

            case 'H':
               (gs -> sp) += 4;
               stackp = ADDRSTACK (gs -> sp);
               intp = (int *) stackp;

               return_addr = gs -> display [gs -> level] + 72;
               *intp = STOR_H (return_addr);

               break;

            case 'B':
            case 'C':
               (gs -> sp) += 4;
               stackp = ADDRSTACK (gs -> sp);
               intp = (int *) stackp;
               charp = (char *) stackp;

               return_addr = gs -> display [gs -> level] + 72;
               *intp = 0;
               *charp = STOR_C (return_addr);

               break;

            case 'R':
               (gs -> sp) += 4;
               stackp = ADDRSTACK (gs -> sp);
               doublep = (double *) stackp;

               (gs -> sp) += 4;
               STACKTYPE (gs -> sp) = 'R';

               return_addr = gs -> display [gs -> level] + 72;
               *doublep = STOR_R (return_addr);

               break;

            case 'P':
               break;

            default:
               BREMSE ("+++ default case !!\n");
               break;
         }

         /************************************************/
         /*   if return from main, stop execution        */
         /************************************************/

         if (gs -> pcups == 0)
         {
            gs -> stepanz = -10;
            incr_ip = 0;
            break;
         }

         /************************************************/
         /*   get old CUPS (CUP save area)               */
         /************************************************/

         pcup = ADDRSTOR (gs -> pcups);

#if 0

         fprintf (stderr, "RET: ------------------------"
                          "---------------------------\n");

         fprintf (stderr, "RET: gs -> pcups = %d\n", gs -> pcups);
         fprintf (stderr, "RET: gs -> level = %d\n", gs -> level);
         fprintf (stderr, "RET: level_caller = %d\n",
                  pcup -> level_caller);
         fprintf (stderr, "RET: pcup -> backchain = %d\n",
                  pcup -> backchain);

         if (pcup -> backchain != 0)
         {
            pcupv = ADDRSTOR (pcup -> backchain);

            for (;;)
            {
               fprintf (stderr, "RET: &BackStatic = %p\n",
                        &(pcupv -> back_static));
               fprintf (stderr, "RET: BackStatic = %d\n",
                        pcupv -> back_static);

               if (pcupv -> back_static == 0)
                  break;

               pcupv = ADDRSTOR (pcupv -> back_static);
            }
         }

#endif

         /************************************************/
         /*   if level of caller greater than            */
         /*   actual level, some displays must be        */
         /*   restored                                   */
         /************************************************/

         if (pcup -> level_caller >= gs -> level)
         {
            int newlevel;

            newlevel = pcup -> level_caller;
            gs -> display [newlevel] = pcup -> backchain;

            /************************************************/
            /*   restore displays from static backchain     */
            /************************************************/

            pcupv = ADDRSTOR (pcup -> backchain);
            newlevel --;

            while (newlevel >= gs -> level)
            {
               gs -> display [newlevel] = pcupv -> back_static;
               pcupv = ADDRSTOR (pcupv -> back_static);
               newlevel --;
            }
         }

         gs -> level = pcup -> level_caller;

         /************************************************/
         /*   trace displays up to new level             */
         /************************************************/

#if 0

         for (i = 1; i <= gs -> level; i ++)
            fprintf (stderr, "RET: Level = %d Display = %d\n",
                     i, gs -> display [i]);

#endif

         /************************************************/
         /*   branch to return addr                      */
         /************************************************/

         gs -> ip = pcup -> returnaddr;
         incr_ip = 0;

         /************************************************/
         /*   activate previous cups                     */
         /************************************************/

         gs -> pcups = pcup -> backchain;

#if 0

         fprintf (stderr, "RET: oldcups = %d\n", gs -> pcups);

#endif

         break;

      case XXX_RND:

         /************************************************/
         /*   gibt's nicht mehr, ist jetzt CSP           */
         /************************************************/

         STACKTYPE (gs -> sp) = ' ';

         dwert = STACK_R ((gs -> sp) - 4);

         (gs -> sp) -= 4;
         STACKTYPE (gs -> sp) = ' ';

         intp = ADDRSTACK (gs -> sp);
         *intp = roundx (dwert, 0);

         break;

      case XXX_RST:

         /************************************************/
         /*   get values from top of stack               */
         /*   (value is old heap pointer)                */
         /************************************************/

         wert = STACK_I (gs -> sp);
         (gs -> sp) -= 4;

         gs -> hp = wert;

         break;

      case XXX_SAV:

         /************************************************/
         /*   get address from top of stack              */
         /************************************************/

         addr = STACK_I (gs -> sp);
         storep = ADDRSTOR (addr);
         (gs -> sp) -= 4;

         intp = (int *) storep;
         *intp = (gs -> hp);

         break;

      case XXX_SBA:

         /************************************************/
         /*   subtract values (addresses) at top of st.  */
         /************************************************/

         wert1 = STACK_I (gs -> sp);
         (gs -> sp) -= 4;
         intp = ADDRSTACK (gs -> sp);
         wert2 = *intp;
         *intp = wert2 - wert1;

         break;

      case XXX_SBI:

         /************************************************/
         /*   subtract values from top of stack          */
         /************************************************/

         wert1 = STACK_I (gs -> sp);
         (gs -> sp) -= 4;
         intp = ADDRSTACK (gs -> sp);
         wert2 = *intp;
         *intp = wert2 - wert1;

         break;

      case XXX_SBR:

         /************************************************/
         /*   subtract real values from top of stack     */
         /*   remove R flag from top stack element       */
         /*   reduce SP by 8 (because of real value)     */
         /************************************************/

         dwert1 = STACK_R ((gs -> sp) - 4);

         STACKTYPE (gs -> sp) = ' ';
         (gs -> sp) -= 8;

         doublep = ADDRSTACK ((gs -> sp) - 4);
         dwert2 = *doublep;
         *doublep = dwert2 - dwert1;

         break;

      case XXX_SCL:

         // Opp / 12.2016
         // working sets relative to pcups

         /************************************************/
         /*   set clear                                  */
         /*   pcode -> p = length                        */
         /*   pcode -> q = addr                          */
         /************************************************/

         len1 = pcode -> p;
         addr = gs -> pcups + pcode -> q;
         charp = ADDRSTOR (addr);
         memset (charp, 0x00, len1);

         /************************************************/
         /*   set representation to stack                */
         /************************************************/

         (gs -> sp) += 4;
         intp = ADDRSTACK (gs -> sp);
         *intp = SET_ON_STACK (addr, len1);

#if 0

         fprintf (stderr, "SCL: len1 = %d\n", len1);
         fprintf (stderr, "SCL: addr = %d\n", addr);

#endif

         break;

      case XXX_SLD:

         // Opp / 12.2016
         // working sets relative to pcups

         /************************************************/
         /*   load set to stack (2 items)                */
         /*   length from SLD instruction                */
         /*   address from SLD instruction               */
         /*   move set representation (bit string)       */
         /*   from source (address from stack) to        */
         /*   target                                     */
         /************************************************/

         /************************************************/
         /*   indirect access by addr at top of stack    */
         /************************************************/

         intp = ADDRSTACK (gs -> sp);
         addr1 = *intp;
         len1 = SET_LEN (addr1);
         addr1 = SET_ADDR (addr1);
         charp = ADDRSTOR (addr1);

         len2 = pcode -> p;
         addr = gs -> pcups + pcode -> q;
         gs -> effadr = addr;

         /************************************************/
         /*   set representation to stack                */
         /************************************************/

         *intp = SET_ON_STACK (addr, len2);

         /************************************************/
         /*   move set representation                    */
         /************************************************/

         charp2 = ADDRSTOR (addr);

         // if (len2 > len1)
         //   memset (charp2 + len1, 0x00, len2 - len1);

         if (len2 > 0)
            memcpy (charp2, charp, len2);

#if 0

         fprintf (stderr, "SLD: len1 = %d\n", len1);
         fprintf (stderr, "SLD: addr1 = %d\n", addr1);
         fprintf (stderr, "SLD: len2 = %d\n", len2);
         fprintf (stderr, "SLD: addr = %d\n", addr);

#endif

         break;

      case XXX_SMV:

         // Opp / 12.2016
         // working sets relative to pcups

         /************************************************/
         /*   move set to storage                        */
         /*   len1 = target length of set                */
         /*   len2 = source length                       */
         /*   if len1 < 0:                               */
         /*   different sequence of parameter on stack   */
         /************************************************/

         len1 = pcode -> p;
         len2 = pcode -> q;

         if (len1 < 0)
         {
            len1 = - len1;

            intp2 = ADDRSTACK (gs -> sp);
            charp2 = ADDRSTOR (SET_ADDR (*intp2));

            (gs -> sp) -= 4;

            intp = ADDRSTACK (gs -> sp);
            charp = ADDRSTOR (SET_ADDR (*intp));

            (gs -> sp) -= 4;
         }
         else
         {
            intp = ADDRSTACK (gs -> sp);
            charp = ADDRSTOR (SET_ADDR (*intp));

            (gs -> sp) -= 4;

            intp2 = ADDRSTACK (gs -> sp);
            charp2 = ADDRSTOR (SET_ADDR (*intp2));

            (gs -> sp) -= 4;
         }

         if (len1 > len2)
         {
            //*************************************
            // ueberzaehlige Laenge auf Hex Null setzen
            //*************************************

            memset (charp2 + len2, 0x00, len1 - len2);
         }

         if (len2 != 0 && charp != charp2)
         {
            len = len1;
            if (len1 > len2)
            {
               len = len2;
            }

            //*************************************
            // wg. moeglicher Ueberlappungen
            //*************************************

            memcpy (setbuffer, charp, len);
            memcpy (charp2, setbuffer, len);
         }

#if 0

         fprintf (stderr, "SMV: pcode -> p = %d\n", pcode -> p);
         fprintf (stderr, "SMV: len1 = %d\n", len1);
         fprintf (stderr, "SMV: len2 = %d\n", len2);
         fprintf (stderr, "SMV: target = %d\n", *intp2);

#endif

         break;

      case XXX_SQI:
         BREMSE ("+++ not implemented !!\n");
         break;

      case XXX_SQR:
         BREMSE ("+++ not implemented !!\n");
         break;

      case XXX_STO:

         if (pcode -> t == 'R')
         {
            /************************************************/
            /*   get value from top of stack                */
            /************************************************/

            dwert = STACK_R ((gs -> sp) - 4);

            /************************************************/
            /*   decrement stack pointer                    */
            /************************************************/

            (gs -> sp) -= 8;
         }
         else
         {
            /************************************************/
            /*   get value from top of stack                */
            /************************************************/

            wert = STACK_I (gs -> sp);

            /************************************************/
            /*   decrement stack pointer                    */
            /************************************************/

            (gs -> sp) -= 4;
         }

         /************************************************/
         /*   get address from top of stack              */
         /************************************************/

         addr = STACK_I (gs -> sp);
         gs -> effadr = addr;
         storep = ADDRSTOR (addr);

         switch (pcode -> t)
         {
            case 'B':
            case 'C':
               *storep = wert;
               break;
            case 'H':
               shortp = (short *) storep;
               *shortp = wert;
               break;
            case 'N':
               voidp = (void **) storep;
               *voidp = NULL;
               break;
            case 'A':
            case 'I':
               intp = (int *) storep;
               *intp = wert;
               break;
            case 'R':
               doublep = (double *) storep;
               *doublep = dwert;
               break;
            default:
               BREMSE ("+++ default case !!\n");
               break;
         }

         /************************************************/
         /*   decrement stack pointer once more          */
         /************************************************/

         (gs -> sp) -= 4;

         break;

      case XXX_STP:
         gs -> stepanz = -10;
         incr_ip = 0;
         break;

      case XXX_STR:

         if (pcode -> t != 'R')
         {
            /************************************************/
            /*   get value from top of stack                */
            /************************************************/

            stackp = ADDRSTACK (gs -> sp);
            intp = (int *) stackp;
            wert = *intp;
         }
         else
         {
            (gs -> sp) -= 4;

            /************************************************/
            /*   get real value from top of stack           */
            /************************************************/

            stackp = ADDRSTACK (gs -> sp);
            doublep = (double *) stackp;
            dwert = *doublep;
         }

         /************************************************/
         /*   store value into level and address         */
         /************************************************/

         disp = gs -> display [pcode -> p];
         addr = disp + pcode -> q;
         gs -> effadr = addr;

         storep = ADDRSTOR (addr);

         switch (pcode -> t)
         {
            case 'B':
            case 'C':
               *storep = wert;
               break;
            case 'H':
               shortp = (short *) storep;
               *shortp = wert;
               break;
            case 'N':
               voidp = (void **) storep;
               *voidp = NULL;
               break;
            case 'A':
            case 'I':
               intp = (int *) storep;
               *intp = wert;
               break;

            case 'R':
               *((double *) storep) = dwert;
               break;

            default:
               BREMSE ("+++ default case !!\n");
               break;
         }

         /************************************************/
         /*   decrement stack pointer                    */
         /************************************************/

         (gs -> sp) -= 4;

         break;

      case XXX_TRC:

         /************************************************/
         /*   gibt's nicht mehr, ist jetzt CSP           */
         /************************************************/

         STACKTYPE (gs -> sp) = ' ';

         dwert = STACK_R ((gs -> sp) - 4);

         (gs -> sp) -= 4;
         STACKTYPE (gs -> sp) = ' ';

         intp = ADDRSTACK (gs -> sp);
         if (dwert >= 0.0)
         {
            *intp = (int) (floor (dwert));
         }
         else
         {
            *intp = (int) (- floor (- dwert));
         }

         break;

      case XXX_UJP:
         gs -> ip = pcode -> q;
         incr_ip = 0;
         break;

      case XXX_UNI:

         /************************************************/
         /*   set union                                  */
         /************************************************/
         /*   korrektur am 18.12.2016:                   */
         /*   der groessere von den beiden operanden     */
         /*   wird der ziel-set und bleibt uebrig;       */
         /*   der andere wird gepoppt                    */
         /************************************************/

         // Opp / 12.2016
         // working sets relative to pcups

         addr = STACK_I (gs -> sp);
         setp1 = ADDRSTOR (SET_ADDR (addr));
         len1 = SET_LEN (addr);

         (gs -> sp) -= 4;

         addr = STACK_I (gs -> sp);
         setp2 = ADDRSTOR (SET_ADDR (addr));
         len2 = SET_LEN (addr);

         if (len2 >= len1)
         {
            len = len1;

            for (cpu1 = setp1, cpu2 = setp2;
                 len > 0;
                 cpu1 += 1, cpu2 += 1, len -= 1)
            {
               *cpu2 |= *cpu1;
            }
         }
         else
         {
            len = len2;

            for (cpu1 = setp1, cpu2 = setp2;
                 len > 0;
                 cpu1 += 1, cpu2 += 1, len -= 1)
            {
               *cpu1 |= *cpu2;
            }

            stackp = ADDRSTACK (gs -> sp);
            intp = (int *) stackp;
            intp [0] = intp [1];
         }

         break;

      case XXX_UXJ:

         pcup = ADDRSTOR (gs -> pcups);

#if 0

         fprintf (stderr, "UXJ: ------------------------"
                          "---------------------------\n");

         fprintf (stderr, "UXJ: gs -> pcups = %d\n", gs -> pcups);
         fprintf (stderr, "UXJ: gs -> level = %d\n", gs -> level);
         fprintf (stderr, "UXJ: level_caller = %d\n",
                  pcup -> level_caller);
         fprintf (stderr, "UXJ: pcup -> backchain = %d\n",
                  pcup -> backchain);

#endif

         backs = pcup -> back_static;
         pcupv = ADDRSTOR (backs);

         for (;;)
         {

#if 0

            fprintf (stderr, "UXJ: pcupv = %p\n",
                     pcupv);
            fprintf (stderr, "UXJ: pcupv -> level_called = %d\n",
                     pcupv -> level_called);

            fprintf (stderr, "UXJ: pcupv -> level_caller = %d\n",
                     pcupv -> level_caller);
            fprintf (stderr, "UXJ: &BackStatic = %p\n",
                     &(pcupv -> back_static));
            fprintf (stderr, "UXJ: BackStatic = %d\n",
                     pcupv -> back_static);

#endif

            if (pcupv -> level_called == pcode -> p)
               break;

            if (pcupv -> back_static == 0)
               runtime_error (gs, BADLBRANCH, NULL);

            backs = pcupv -> back_static;
            pcupv = ADDRSTOR (backs);
         }

         gs -> level = pcode -> p;
         gs -> pcups = backs;

         gs -> ip = pcode -> q;
         incr_ip = 0;

         break;

      case XXX_XLB:
         break;

      case XXX_XJP:

         /************************************************/
         /*   get value from top of stack                */
         /************************************************/

         wert = STACK_I (gs -> sp);

         // printf ("+++ XJP Wert = %d\n", wert);

         /************************************************/
         /*   label 1 points to branch table             */
         /*   label 2 points to default branch target    */
         /************************************************/

         label1 = pcode -> q;
         pcode1 = gs -> code0 + label1;

         label2 = pcode -> x;

         if (pcode -> psect == NULL)
         {
            /************************************************/
            /*   build "real" branch table                  */
            /************************************************/

            int cmin;
            int cmax;
            int i;

            //************************************************
            // erste DEF Konstante suchen
            //************************************************

            pcodex = pcode1;
            while (gs -> ot [pcodex -> op] . opnum != XXX_DEF)
               pcodex ++;

            cmin = pcodex -> q;
            cmax = pcodex -> q;

            //************************************************
            // kleinsten und groessten Wert ermitteln
            //************************************************

            for (;; pcodex ++)
            {
               if (gs -> ot [pcodex -> op] . opnum == XXX_LAB)
               {
                  pcodez = pcodex;
                  break;
               }

               if (gs -> ot [pcodex -> op] . opnum == XXX_DEF)
               {
                  if (cmin > pcodex -> q)
                     cmin = pcodex -> q;
                  if (cmax < pcodex -> q)
                     cmax = pcodex -> q;
               }
            }

            // printf ("+++ XJP cmin = %d\n", cmin);
            // printf ("+++ XJP cmax = %d\n", cmax);

            //************************************************
            // alloc fuer branch table
            //************************************************

            pcode -> psect = malloc ((CIXMAX + 2) * sizeof (int));
            btable = (int *) (pcode -> psect);

            //************************************************
            // kleinsten und groessten Wert und
            // default labels ablegen
            //************************************************

            btable [0] = cmin;
            btable [1] = cmax;
            for (i = 2; i < CIXMAX + 2; i ++)
               btable [i] = label2;

            //************************************************
            // konkrete Labels fuer konkrete Werte eintragen
            //************************************************

            for (pcodex = pcode1;
                 pcodex <= pcodez;
                 pcodex ++)
            {
               if (gs -> ot [pcodex -> op] . opnum == XXX_DEF)
               {
                  defconst = pcodex -> q;
                  btarget = (pcodex + 1) -> q;
                  btable [defconst - cmin + 2] = btarget;

                  // printf ("+++ XJP defconst = %d\n", defconst);
                  // printf ("+++ XJP btarget = %d\n", btarget);
               }
            }
         }

         //************************************************
         // branch table ist schon da, also jetzt
         // Sprungziel zu case label aus branch table holen
         // oder default, if case label ausserhalb der limits
         //************************************************

         btable = (int *) (pcode -> psect);

         if (wert < btable [0] || wert > btable [1])
         {
            gs -> ip = label2;

            // printf ("+++ label2 = %d\n", label2);

            incr_ip = 0;
         }
         else
         {
            gs -> ip = btable [wert - btable [0] + 2];

            // printf ("+++ btarget = %d\n", gs -> ip);

            incr_ip = 0;
         }

         /************************************************/
         /*   decrement stack pointer                    */
         /************************************************/

         (gs -> sp) -= 4;

         break;

      case XXX_XOR:

         /************************************************/
         /*   XOR top 2 values from stack                */
         /************************************************/

         if (pcode -> t == 'B')
         {
            bool1 = STACK_C (gs -> sp);
            (gs -> sp) -= 4;
            intp = ADDRSTACK (gs -> sp);
            charp = (char *) intp;
            bool2 = *charp;
            *intp = (bool1 != 0) != (bool2 != 0);
         }
         else if (pcode -> t == 'I')
         {
            wert1 = STACK_I (gs -> sp);
            (gs -> sp) -= 4;
            intp = ADDRSTACK (gs -> sp);
            wert2 = *intp;
            *intp = wert1 ^ wert2;
         }
         else
         {
            BREMSE ("+++ default case !!\n");
         }

         break;

      default:
         BREMSE ("+++ not implemented !!\n");
         break;
   }

   if (incr_ip)
      (gs -> ip) ++;

   if (gs -> sp < 16)
      runtime_error (gs, STACKNEG, NULL);
}





static void show (global_store *gs, int nur_pascal)

/**********************************************************/
/*                                                        */
/*   Zeige auszufuehrenden Befehl usw. an                 */
/*                                                        */
/**********************************************************/

{
   sc_code *pcode;
   char *plabel;
   char *poper;
   ent_section *pent;

   if (nur_pascal == 0)
   {
      fprintf (stderr, "ip=%06d sp=%08d hp=%08d",
               gs -> ip, gs -> sp, gs -> hp);
      if (gs -> effadr != 0)
      {
         fprintf (stderr, " addr=%08d/%08x",
                  gs -> effadr, gs -> effadr);
      }
      fprintf (stderr, "\n");
   }

   pcode = gs -> code0 + gs -> ip;
   plabel = pcode -> plabel;
   poper = pcode -> poper;

   if (nur_pascal == 0)
   {
      dump_stack (stderr, NULL,
                  gs -> stack0,
                  gs -> sp - 16,
                  gs -> sp + 16,
                  0);

      fprintf (stderr, "%06d: %03d %c %5d %9d  %-8s %-3s %s\n",
              pcode - gs -> code0,
              pcode -> op,
              pcode -> t,
              pcode -> p,
              pcode -> q,
              (plabel != NULL ? plabel : ""),
              gs -> ot [pcode -> op] . opcode,
              (poper != NULL ? poper : ""));
   }

   if (memcmp (gs -> ot [pcode -> op] . opcode, "ENT", 3) == 0)
   {
      pent = pcode -> psect;
      fprintf (stderr,
               "*** LOC %4d: %s %s %s\n",
               pcode -> loc, "==> ENT",
               pent -> name_short, pent -> name_long);
   }

   if (memcmp (gs -> ot [pcode -> op] . opcode, "RET", 3) == 0)
   {
      pent = pcode -> psect;
      fprintf (stderr,
               "*** LOC %4d: %s\n",
               pcode -> loc, "<== RET");
   }

   if (pcode -> psource != NULL)
   {
      fprintf (stderr,
               "*** LOC %4d: %s\n",
               pcode -> loc, pcode -> psource);
   }
}





static void init_std_names (global_store *gs)

/**********************************************************/
/*                                                        */
/*   Initialisierung Standard-Daten                       */
/*                                                        */
/*   z.B. globale Files                                   */
/*                                                        */
/**********************************************************/

{
   filecb *fcb;
   int adrfcb;
   int *intp;

   /****************************************************/
   /*   248 = INPUT                                    */
   /*   260 = OUTPUT                                   */
   /*   272 = PRD                                      */
   /*   284 = PRR                                      */
   /*   296 = QRD                                      */
   /*   308 = QRR                                      */
   /****************************************************/

   adrfcb = next_fcb (gs);
   fcb = (filecb *) (gs -> store0 + adrfcb);
   memcpy (fcb, &nullfcb_text, sizeof (filecb));
   strcpy (fcb -> ddname, "INPUT");
   fcb -> inout = 'I';
   fcb -> eof = 0;
   fcb -> pstore = 248;
   fcb -> terminal = 'N';
   intp = ADDRSTOR (fcb -> pstore);
   *intp = adrfcb;

   adrfcb = next_fcb (gs);
   fcb = (filecb *) (gs -> store0 + adrfcb);
   memcpy (fcb, &nullfcb_text, sizeof (filecb));
   strcpy (fcb -> ddname, "OUTPUT");
   fcb -> inout = 'O';
   fcb -> eof = 1;
   fcb -> pstore = 260;
   fcb -> terminal = 'N';
   intp = ADDRSTOR (fcb -> pstore);
   *intp = adrfcb;

   adrfcb = next_fcb (gs);
   fcb = (filecb *) (gs -> store0 + adrfcb);
   memcpy (fcb, &nullfcb_text, sizeof (filecb));
   strcpy (fcb -> ddname, "PRD");
   fcb -> pstore = 272;
   fcb -> terminal = 'N';
   intp = ADDRSTOR (fcb -> pstore);
   *intp = adrfcb;

   adrfcb = next_fcb (gs);
   fcb = (filecb *) (gs -> store0 + adrfcb);
   memcpy (fcb, &nullfcb_text, sizeof (filecb));
   strcpy (fcb -> ddname, "PRR");
   fcb -> pstore = 284;
   fcb -> terminal = 'N';
   intp = ADDRSTOR (fcb -> pstore);
   *intp = adrfcb;

   adrfcb = next_fcb (gs);
   fcb = (filecb *) (gs -> store0 + adrfcb);
   memcpy (fcb, &nullfcb_text, sizeof (filecb));
   strcpy (fcb -> ddname, "QRD");
   fcb -> pstore = 296;
   fcb -> terminal = 'N';
   intp = ADDRSTOR (fcb -> pstore);
   *intp = adrfcb;

   adrfcb = next_fcb (gs);
   fcb = (filecb *) (gs -> store0 + adrfcb);
   memcpy (fcb, &nullfcb_text, sizeof (filecb));
   strcpy (fcb -> ddname, "QRR");
   fcb -> pstore = 308;
   fcb -> terminal = 'N';
   intp = ADDRSTOR (fcb -> pstore);
   *intp = adrfcb;
}




static void store_cmdline (global_store *gs, char *user_cmdline)

{
   int *intp;
   char *charp;

   intp = ADDRSTOR (gs -> cmdline);
   charp = ADDRSTOR (gs -> cmdline + 4);
   *intp = strlen (user_cmdline);
   memcpy (charp, user_cmdline, *intp);
   gs -> actsize_cmdline = *intp;
}




static void interpreter (global_store *gs)

/**********************************************************/
/*                                                        */
/*   Interpreter                                          */
/*                                                        */
/**********************************************************/

{
   int addr1;
   int addr2;
   int anzahl;

   char zeile [85];
   char cmd [81];
   char p1 [80];
   char p2 [80];
   char p3 [80];
   char p4 [80];
   char break_arg [80];
   char addr_arg [80];
   char sch_break;
   char sch_trace;
   char sch_traceproc;
   int *intp;

   int is_ret;
   int is_break;
   int callno;

   char mode = 'P';   // Assembler (P-Code) oder Pascal

   gs -> ip = gs -> startpos;
   gs -> sp = 16;
   gs -> hp = gs -> start_const - 16;
   gs -> level = 1;
   gs -> pcups = 0;

   /**********************************************************/
   /*   Date-String an feste Speicherposition                */
   /*   Time-String an feste Speicherposition                */
   /*   OSPARM auf NIL (zunaechst)                           */
   /**********************************************************/

   memcpy (ADDRSTOR (328), gs -> date_string, 10);
   memcpy (ADDRSTOR (338), gs -> date_string + 11, 10);

   if (gs -> actsize_cmdline == 0)
   {
      intp = ADDRSTOR (348);
      *intp = -1;
   }
   else
   {
      intp = ADDRSTOR (348);
      *intp = gs -> cmdline;
   }

   /**********************************************************/
   /*   Clock initialisieren                                 */
   /**********************************************************/

   used_millisecs (1);

   for (;;)
   {
      //***************************************************
      //   im Debug-Fall:
      //   Kommando und bis zu vier Parameter einlesen
      //***************************************************

      if (gs -> sch_debug != 'N')
      {
         show (gs, 0);

         fgets (zeile, 80, stdin);
         anzahl = sscanf (zeile,
                          "%s %s %s %s %s",
                          cmd, p1, p2, p3, p4);

         if (anzahl < 5)
            *p4 = 0x00;

         if (anzahl < 4)
            *p3 = 0x00;

         if (anzahl < 3)
            *p2 = 0x00;

         if (anzahl < 2)
            *p1 = 0x00;
      }

      //***************************************************
      //   andernfalls: Kommando G und keine Parameter
      //***************************************************

      else
      {
         strcpy (cmd, "G");
         anzahl = 1;
         *p4 = 0x00;
         *p3 = 0x00;
         *p2 = 0x00;
         *p1 = 0x00;
      }

#if 0

      fprintf (stderr,
               "*** Kommando gelesen: (%d) "
               "cmd=<%s> p1=<%s> p2=<%s> p3=<%s> p4=<%s>\n",
               anzahl, cmd, p1, p2, p3, p4);

#endif

      gs -> stepanz = 1;

      //***************************************************
      //   Kommandos abarbeiten
      //   E = Ende der Verarbeitung
      //***************************************************

      s_toupper (cmd);

      if (strcmp (cmd, "E") == 0)
      {
         break;
      }

      //***************************************************************
      //   X oder T = Trace
      //   T = mit Anzeige der Zwischenschritte
      //   TP = mit Anzeige der Prozeduraufrufe
      //   X = ohne
      //   in beiden Faellen muessen Abbruchkriterien
      //   angegeben werden:
      //   R oder RET, d.h. wenn Funktionsende erreicht wird
      //   F=funktionsname, d.h. beim Erreichen einer best. Funktion
      //   L=nnn, d.h. beim Erreichen einer Statememtnummer
      //***************************************************************

      else if (strcmp (cmd, "X") == 0 ||
               strcmp (cmd, "T") == 0 ||
               strcmp (cmd, "TP") == 0)
      {
         sc_code *pcoden;
         ent_section *pent;

         gs -> stepanz = 999999999;

         sch_trace = (strcmp (cmd, "T") == 0);
         sch_traceproc = (strcmp (cmd, "TP") == 0);
         sch_break = ' ';

         if (strcmp_ignore (p1, "RET") == 0 ||
             strcmp_ignore (p1, "R") == 0)
         {
            sch_break = 'R';
         }
         else if (memcmp_ignore (p1, "F=", 2) == 0)
         {
            sch_break = 'F';
            strcpy (break_arg, p1 + 2);
            s_toupper (break_arg);
         }
         else if (memcmp_ignore (p1, "L=", 2) == 0)
         {
            sch_break = 'L';
            strcpy (break_arg, p1 + 2);
         }
         else
         {
            fprintf (stderr, "*** Parameter: ret, f=, l=\n");
            continue;
         }

         callno = 0;

#if 0

         fprintf (stderr,
                  "*** sch_trace = %d\n", sch_trace);

#endif

         for (;;)
         {
            int1 (gs);

            if (gs -> stepanz <= 0)
               break;

            pcoden = gs -> code0 + gs -> ip;
            is_ret = 0;

            switch (sch_break)
            {
               case 'R':
                  if (memcmp (gs -> ot [pcoden -> op] . opcode,
                              "RET", 3) == 0)
                  {
                     is_ret = 1;

                     callno --;
                     if (callno < 0)
                     {
                        gs -> stepanz = 1;
                     }
                  }

                  if (memcmp (gs -> ot [pcoden -> op] . opcode,
                              "ENT", 3) == 0)
                  {
                     callno ++;
                  }
                  break;

               case 'F':
                  if (memcmp (gs -> ot [pcoden -> op] . opcode,
                              "ENT", 3) == 0)
                  {
                     pent = pcoden -> psect;
                     if (strcmp_ignore (break_arg,
                                        pent -> name_long) == 0)
                     {
                        gs -> stepanz = 1;
                     }
                  }
                  break;

               case 'L':
                  if (atoi (break_arg) == pcoden -> loc)
                  {
                     gs -> stepanz = 1;
                  }
                  break;

               default:
                  break;
            }

            if (sch_trace)
               show (gs, 1);

            if (sch_traceproc)
            {
               if (memcmp (gs -> ot [pcoden -> op] . opcode,
                           "RET", 3) == 0 ||
                   memcmp (gs -> ot [pcoden -> op] . opcode,
                           "ENT", 3) == 0)
               {
                  show (gs, 1);
               }
            }

            if (gs -> stepanz > 1 &&
                pcoden -> psource == NULL &&
                ! is_ret)
               continue;

            (gs -> stepanz) --;
            if (gs -> stepanz <= 0)
               break;
         }

         fprintf (stderr, "\n");

         if (gs -> stepanz < - 9)
         {
            fprintf (stdout, "\n*** Pascal Programm STP ***\n");
            break;
         }
      }

      /**********************************************************/
      /*   Step oder Trace abh. von Modus Pascal oder ASM       */
      /**********************************************************/

      else if (strcmp (cmd, "S") == 0 ||
               strcmp (cmd, "I") == 0 ||
               strcmp (cmd, "B") == 0)
      {
         if (mode == 'A')
         {
            gs -> stepanz = 1;

            for (;;)
            {
               int1 (gs);

               (gs -> stepanz) --;
               if (gs -> stepanz <= 0)
                  break;
            }

            if (gs -> stepanz < - 9)
            {
               fprintf (stdout, "\n*** Pascal Programm STP ***\n");
               break;
            }
         }
         else
         {
            sc_code *pcoden;

            if (anzahl >= 2)
               gs -> stepanz = atoi (p1);
            else if (*cmd == 'S' || *cmd == 'I')
               gs -> stepanz = 1;
            else
               gs -> stepanz = 999999999;

            callno = 0;

            for (;;)
            {
               int1 (gs);

               pcoden = gs -> code0 + gs -> ip;

               is_ret = 0;

               if (memcmp (gs -> ot [pcoden -> op] . opcode,
                           "RET", 3) == 0)
               {
                  is_ret = 1;

                  callno --;
                  if (callno < 0)
                  {
                     if (*cmd == 'B')
                        gs -> stepanz = 1;
                  }
               }

               if (memcmp (gs -> ot [pcoden -> op] . opcode,
                           "ENT", 3) == 0)
               {
                  if (*cmd != 'I')
                     callno ++;
               }

               if (pcoden -> psource == NULL && ! is_ret)
                  continue;

               if (callno > 0)
                  continue;

#if 0
               fprintf (stderr, "*** stepanz = %d callno = %d\n",
                                gs -> stepanz, callno);
#endif

               (gs -> stepanz) --;
               if (gs -> stepanz <= 0)
                  break;

               show (gs, 1);
            }

            fprintf (stderr, "\n");

            if (gs -> stepanz < - 9)
            {
               fprintf (stdout, "\n*** Pascal Programm STP ***\n");
               break;
            }
         }
      }

      /**********************************************************/
      /*   Ablauf (Go) nicht ueberwacht                         */
      /**********************************************************/

      else if (strcmp (cmd, "G") == 0)
      {
         if (anzahl >= 2)
         {
            //**********************************************
            //*   Anzahl Steps vorgegeben
            //**********************************************

            gs -> stepanz = atoi (p1);

            for (;;)
            {
               int1 (gs);
               (gs -> stepanz) --;
               if (gs -> stepanz <= 0)
                  break;
            }

            if (gs -> stepanz < - 9)
            {
               fprintf (stdout, "\n*** Pascal Programm STP ***\n");
               break;
            }
         }
         else
         {
            //**********************************************
            //*   Endlose Ausfuehrung - Produktion
            //**********************************************

            for (;;)
            {
               int1 (gs);
               if (gs -> stepanz <= 0)
                  break;
            }

            if (gs -> stepanz < - 9)
            {
               break;
            }
         }
      }

      /**********************************************************/
      /*   Display Store                                        */
      /**********************************************************/

      else if (strcmp (cmd, "D") == 0)
      {
         if (*p1 == 'x' || *p1 == 'x')
         {
            sprintf (addr_arg, "0x%s", p1 + 1);
            addr1 = strtol (addr_arg, NULL, 0);
         }
         else
         {
            addr1 = atoi (p1);
         }

         if (*p2 == 'x' || *p2 == 'x')
         {
            sprintf (addr_arg, "0x%s", p2 + 1);
            addr2 = strtol (addr_arg, NULL, 0);
         }
         else if (*p2 == '+')
         {
            if (p2 [1] == 'x' || p2 [1] == 'x')
            {
               sprintf (addr_arg, "0x%s", p2 + 2);
               addr2 = strtol (addr_arg, NULL, 0);

            }
            else
            {
               addr2 = atoi (p2);
            }

            addr2 += addr1;
         }
         else
         {
            addr2 = atoi (p2);
         }

         dump_store (stderr, NULL,
                     gs -> store0,
                     addr1,
                     addr2 - addr1,
                     0);
      }
      else if (strcmp (cmd, "N") == 0)
      {
         (gs -> ip) ++;
      }
      else if (strcmp (cmd, "IP") == 0)
      {
         gs -> ip = atoi (p1);
         fprintf (stderr,
                  "\n*** IP set to %d\n\n", gs -> ip);
      }
      else if (strcmp (cmd, "SP") == 0)
      {
         gs -> sp = atoi (p1);
         fprintf (stderr,
                  "\n*** SP set to %d\n\n", gs -> sp);
      }
      else if (strcmp (cmd, "HP") == 0)
      {
         gs -> hp = atoi (p1);
         fprintf (stderr,
                  "\n*** HP set to %d\n\n", gs -> hp);
      }
      else if (strcmp (cmd, "ASM") == 0)
      {
         mode = 'A';
         fprintf (stderr,
                  "\n*** Mode ASM set\n\n");
      }
      else if (strcmp (cmd, "PAS") == 0)
      {
         mode = 'P';
         fprintf (stderr,
                  "\n*** Mode PAS set\n\n");
      }
      else if (strcmp (cmd, "\?") == 0)
      {
         fprintf (stderr,

   "\n"
   "/**********************************************************/\n"
   "/*                                                        */\n"
   "/*   Kommandos:                                           */\n"
   "/*                                                        */\n"
   "/*   e = ende                                             */\n"
   "/*   x = Execute Pascal Befehle bis Event                 */\n"
   "/*   g = go                                               */\n"
   "/*   s = Step (abh. von Modus Pascal oder ASM)            */\n"
   "/*   i = Step Into (gehe in Prozedur - Pascal)            */\n"
   "/*   t = Trace (abh. von Modus Pascal oder ASM)           */\n"
   "/*   b = Step until Return (= Back - Pascal)              */\n"
   "/*                                                        */\n"
   "/*   d = display store                                    */\n"
   "/*       Beispiele:                                       */\n"
   "/*       d adr1 adr2     von adr1 bis adr2                */\n"
   "/*       d adr  +len     von adr in der Laenge len        */\n"
   "/*       Adressen koennen dezimal oder hex angegeben      */\n"
   "/*       werden (mit Praefix x)                           */\n"
   "/*                                                        */\n"
   "/*   n   = naechster befehl (ip plus eins setzen)         */\n"
   "/*   ip  = zum setzen von ip                              */\n"
   "/*   sp  = zum setzen von sp                              */\n"
   "/*   mp  = zum setzen von mp                              */\n"
   "/*   hp  = zum setzen von hp                              */\n"
   "/*   pas = zum Setzen von Mode Pascal                     */\n"
   "/*   asm = zum Setzen von Mode Assembler                  */\n"
   "/*                                                        */\n"
   "/*   Events bei T(race) bzw. S(tep):                      */\n"
   "/*   ret       return                                     */\n"
   "/*   f=xxx     bis zur Funktion mit Name = xxx            */\n"
   "/*   l=nnn     bis zur Location nnn                       */\n"
   "/*                                                        */\n"
   "/**********************************************************/\n"
   "\n"

                 );
      }
      else
      {
         fprintf (stderr,
                  "\n+++ Command not recognized, \? for help\n\n");
      }
   }
}






int main (int argc, char **argv)

/**********************************************************/
/*                                                        */
/*   Hauptprogramm PCINT                                  */
/*                                                        */
/**********************************************************/

{
   int rc;
   char *cp;
   char *cpnext;

   char inpfilename [65];
   char includes [257];
   char pasfilename [65];
   char outfilename [65];
   int parm_sconst;
   int parm_files;
   int parm_newheap;
   char incfilename [65];
   char incfilepasname [65];

   FILE *inpfile;
   FILE *outfile;

   char version_string [32];

   global_store gs;

   char user_cmdline [CMDLINEMAX];

   /**********************************************************/
   /*   fuer CMD-Line-Parameter                              */
   /**********************************************************/

   int parmposition;
   char *parmfehlerpos;

   static char *parmkeywords [] = { "prr=",
                                    "inc=",
                                    "pas=",
                                    "out=",
                                    "sconst=",
                                    "files=",
                                    "newheap=",
                                    "sizecmd=",
                                    "debug=",
                                    "list=" };

#define PARM_ANZAHL   (sizeof(parmkeywords) / sizeof(char *))

   char *parmvalues [PARM_ANZAHL] = { "",
                                      "",
                                      "",
                                      "",
                                      "3000000",
                                      "100",
                                      "6000000",
                                      "512",
                                      "N",
                                      "N" };

   static int parmmaxlaenge [PARM_ANZAHL] = { 64, 256, 64, 64,
                                               8, 8, 8, 8, 1, 1 };

   static int parmpruefungen [PARM_ANZAHL] = { 0, 0, 0, 0,
                                               2, 2, 2, 2, 1, 1 };

   /**********************************************************/
   /*   Ende Definitionen, Start ausfuehrbare Anweisungen    */
   /**********************************************************/

   sprintf (version_string, "%s %s %s",
                            PCINT_VERSION, __DATE__, __TIME__);

   if (argc == 1)
   {
      fprintf (stdout, "\nPCINT (Build %s)\n", version_string);
      fprintf (stdout, "\n");
      fprintf (stdout, "Aufruf: PCINT <Parameter>\n");
      fprintf (stdout, "\n");
      fprintf (stdout, "Die Reihenfolge der Parameter ist beliebig;\n");
      fprintf (stdout, "die einzelnen Parameter und ihre Bedeutung:\n");
      fprintf (stdout, "\n");
      fprintf (stdout, "prr=      PCODE-Eingabedatei\n");
      fprintf (stdout, "inc=      Weitere PCODE-Dateien (Module)\n");
      fprintf (stdout, "pas=      Pascal-Source (falls vorhanden)\n");
      fprintf (stdout, "out=      Name der Ausgabedatei\n");
      fprintf (stdout, "sconst=   Startadresse fuer Konstanten\n");
      fprintf (stdout, "files=    maximale Anzahl Files\n");
      fprintf (stdout, "newheap=  Platz fuer Heap (new Style)\n");
      fprintf (stdout, "sizecmd=  maximale Groesse CMDLINE\n");
      fprintf (stdout, "debug=    Debug-Schalter (J/N)\n");
      fprintf (stdout, "list=     Listing gewuenscht (J/N)\n");
      fprintf (stdout, "\n");
      fprintf (stdout, "Defaults:\n");
      fprintf (stdout, "\n");

      for (parmposition = 0;
           parmposition < PARM_ANZAHL;
           parmposition++)
      {
         if (parmvalues [parmposition] != NULL)
            fprintf (stdout, "%-10.10s%s\n",
                    parmkeywords [parmposition],
                    parmvalues [parmposition]);
      }

      fprintf (stdout, "\n");

      exit (4);
   }

   /************************************************/
   /*   Parameter einlesen und pruefen:            */
   /*   falsche Keywords - Fehler 1                */
   /*   Parameter nicht angegeben - Fehler 2       */
   /*   Wert zu lang - Fehler 3                    */
   /************************************************/

   *user_cmdline = 0x00;

   rc = rzp_scanargs (argc,
                      argv,
                      parmkeywords,
                      parmvalues,
                      parmmaxlaenge,
                      parmpruefungen,
                      PARM_ANZAHL,
                      &parmposition,
                      &parmfehlerpos,
                      user_cmdline);

#undef PARM_ANZAHL


   if (rc != 0)
   {
      fprintf (stdout, "\nPCINT (Build %s)\n", version_string);
   }

   switch (rc)
   {
      case 1:
         parm_fehler (1, argv [parmposition], NULL, NULL,
                      NULL, NULL, NULL, NULL);
         return 40;

      case 2:
         parm_fehler (2, parmkeywords [parmposition], NULL, NULL,
                      NULL, NULL, NULL, NULL);
         return 40;

      case 3:
         parm_fehler (3, parmkeywords [parmposition],
                      &(parmmaxlaenge [parmposition]), NULL,
                      NULL, NULL, NULL, NULL);
         return 40;

      case 4:
         parm_fehler (4, parmkeywords [parmposition], NULL, NULL,
                      NULL, NULL, NULL, NULL);
         return 40;

      case 5:
         parm_fehler (5, parmkeywords [parmposition], NULL, NULL,
                      NULL, NULL, NULL, NULL);
         return 40;

      case 6:
         parm_fehler (6, PARMFILE, NULL, NULL,
                      NULL, NULL, NULL, NULL);
         return 40;

      case 7:
         parm_fehler (7, parmfehlerpos, NULL, NULL,
                      NULL, NULL, NULL, NULL);
         return 40;

      default:
         break;
   }

   fprintf (stdout, "\nPCINT (Build %s)\n\n", version_string);

   memset (&gs, 0x00, sizeof (global_store));

   strcpy (inpfilename, parmvalues [0]);
   strcpy (includes, parmvalues [1]);
   strcpy (pasfilename, parmvalues [2]);
   strcpy (outfilename, parmvalues [3]);
   parm_sconst = atoi (parmvalues [4]);
   parm_files = atoi (parmvalues [5]);
   parm_newheap = atoi (parmvalues [6]);
   gs.maxsize_cmdline = atoi (parmvalues [7]);
   gs.sch_debug = *(parmvalues [8]);
   gs.sch_listing = *(parmvalues [9]);

   //**************************************************
   //   Debug geht nur mit Listing an (wg. Source)
   //**************************************************

   if (gs.sch_debug == 'J')
      gs.sch_listing = 'J';

   inpfile = fopen (inpfilename, "r");

   if (inpfile == NULL)
   {
      parm_fehler (6, inpfilename, NULL, NULL,
                   NULL, NULL, NULL, NULL);
      return 20;
   }

   if (gs.sch_listing == 'J')
   {
      outfile = fopen (outfilename, "w");

      if (outfile == NULL)
      {
         parm_fehler (10, outfilename, NULL, NULL,
                      NULL, NULL, NULL, NULL);
         return 20;
      }
   }
   else
   {
      outfile = NULL;
   }

   rzt_sysdate_usa (gs.date_string, 25);

   gs.inpfile = inpfile;
   gs.outfile = outfile;
   gs.ot = ot;
   gs.ft = ft;

   gs.code0 = NULL;
   gs.code_alloc = 0;
   gs.code_used = 0;
   gs.start_const = parm_sconst;
   gs.maxfiles = parm_files;
   gs.sizenewheap = parm_newheap;

   gs.sourcecache = NULL;

   /*****************************************************************/
   /*   translate - die PCODE-Befehle werden in die internen        */
   /*   Strukturen des PCODE-Simulators ueberfuehrt                 */
   /*****************************************************************/

   //*******************************************************
   //*   muss hier stehen bleiben, weil translate
   //*   gs -> sourcename braucht; wird nach
   //*   pent -> sourcename kopiert (fuer jeden gefundenen
   //*   ENT-Befehl)
   //*******************************************************

   if (strlen (pasfilename) > sizeof (gs.sourcename) - 1)
   {
      memcpy (gs.sourcename,
              pasfilename,
              sizeof (gs.sourcename) - 1);
   }
   else
   {
      strcpy (gs.sourcename, pasfilename);
   }

   translate (&gs, gs.inpfile, inpfilename);

   fclose (inpfile);

   /*****************************************************************/
   /*   translate fuer includes, d.h.dazugelinkte Module            */
   /*****************************************************************/

   for (cp = includes; *cp != 0x00; )
   {
      cpnext = strchr (cp, ',');
      if (cpnext != NULL)
      {
         *cpnext = 0x00;
         cpnext += 1;
      }
      else
      {
         cpnext = cp + strlen (cp);
      }

      strcpy (incfilename, cp);
      strcat (incfilename, ".prr");

      strcpy (incfilepasname, cp);
      strcat (incfilepasname, ".pas");

      //*******************************************************
      //*   muss hier stehen bleiben, weil translate
      //*   gs -> sourcename braucht; wird nach
      //*   pent -> sourcename kopiert (fuer jeden gefundenen
      //*   ENT-Befehl)
      //*******************************************************

      if (strlen (incfilepasname) > sizeof (gs.sourcename) - 1)
      {
         memcpy (gs.sourcename,
                 incfilepasname,
                 sizeof (gs.sourcename) - 1);
      }
      else
      {
         strcpy (gs.sourcename, incfilepasname);
      }

      inpfile = fopen (incfilename, "r");
      if (inpfile == NULL)
      {
         fprintf (stderr, "+++ Include File %s could not be opened\n",
                  incfilename);
      }
      else
      {
         gs.inpfile = inpfile;
         translate (&gs, gs.inpfile, incfilename);
         fclose (inpfile);
      }

      inpfile = fopen (incfilepasname, "r");
      if (inpfile == NULL)
      {
         fprintf (stderr,
                  "++ Include Source File %s could not be opened\n",
                  incfilepasname);
      }
      else
      {
         read_pascal (&gs, incfilepasname);
         fclose (inpfile);
      }

      cp = cpnext;
   }

   /*****************************************************************/
   /*   translate2 - fuer ein paar Befehle muessen aus              */
   /*   Performancegruenden noch Nacharbeiten durchgefuehrt         */
   /*   werden                                                      */
   /*****************************************************************/

   translate2 (&gs);

   store_cmdline (&gs, user_cmdline);

   /*****************************************************************/
   /*   read_pascal - Pascal-Source einlesen und Zeilen an          */
   /*   LOC- bzw. ENT-Statements anhaengen                          */
   /*****************************************************************/

   read_pascal (&gs, pasfilename);

   /*****************************************************************/
   /*   aus den internen Strukturen wird ein Listing erzeugt        */
   /*****************************************************************/

   if (gs.sch_listing == 'J')
   {
      listing (&gs);

      fclose (outfile);
   }

   /*****************************************************************/
   /*   Start Interpreter                                           */
   /*****************************************************************/

   init_std_names (&gs);

   interpreter (&gs);

   return rc;
}

