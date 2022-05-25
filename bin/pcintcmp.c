/**********************************************************************/
/*                                                                    */
/*  P-Code Interpreter                                                */
/*                                                                    */
/*  here: parts of P-Code Assembler                                   */
/*                                                                    */
/*  Oppolzer / from 2012 until today                                  */
/*                                                                    */
/**********************************************************************/



#include "pcint.h"





static int hexvalue (char c)

/************************************************/
/*                                              */
/*   hexadezimaler Wert eines chars             */
/*                                              */
/************************************************/

{
   if (c >= '0' && c <= '9')
      return c - '0';

   if (c >= 'A' && c <= 'F')
      return c - 'A' + 10;

   if (c >= 'a' && c <= 'f')
      return c - 'a' + 10;

   return 0;
}





static long intvalue (char *cp)

/************************************************/
/*                                              */
/*   Wert eines Strings der Form INUMBER        */
/*   ermitteln; Problem: Break-Character        */
/*   dabei ueberlesen                           */
/*                                              */
/************************************************/

{
   long wert = 0;

   while (*cp != 0x00)
   {
      if (*cp >= '0' && *cp <= '9')
         wert += wert * 10 + *cp - '0';

      cp ++;
   }

   return wert;
}





static int copy_string (void *vgs,
                        char *cpt,
                        char *cps)

/**********************************************************/
/*                                                        */
/*   Kopiere String                                       */
/*                                                        */
/**********************************************************/

{
   global_store *gs = vgs;

   char buffer [33000];
   char *cptsave;

   int len = 0;
   int len_orig;
   char *cp;
   char mode = ' ';
   int x;
   int i;

   //***************************************************
   //   if X or B present, remember type tag
   //***************************************************

   if (*cps != '\'')
   {
      if (*cps == 'X' || *cps == 'B')
      {
         mode = *cps;
         cps ++;
      }
   }

   //***************************************************
   //   if no apostroph present, show local error
   //***************************************************

   if (*cps != '\'')
      gs -> local_error = 1;

   cps ++;

   //***************************************************
   //   if mode = X or B, move content to local buffer
   //***************************************************

   if (mode != ' ')
   {
      cptsave = cpt;
      cpt = buffer;
   }

   for (;;)
   {
      //***************************************************
      //   take care of double apostrophes and of
      //   line continuations
      //***************************************************

      if (*cps == '\'')
      {
         if (cps [1] == ',')
         {
            cp = fgets (gs -> inpzeile, 255, gs -> inpfile);
            if (cp == NULL)
               break;

            cps = cp;
            while (*cps != '\'')
               cps ++;
            cps ++;

            continue;
         }

         if (cps [1] != '\'')
            break;

         cps ++;
      }

      //***************************************************
      //   if X or B, don't store apostrophes, even if
      //   present (should not be)
      //***************************************************

      if (*cps == '\'' && mode != ' ')
      {
         cps ++;
         continue;
      }

      *cpt = *cps;
      cps ++;
      cpt ++;
      len ++;
   }

   //***************************************************
   //   convert X or B content
   //***************************************************

   if (mode == 'X')
   {
      len_orig = len;

      x = len % 2;
      if (x == 0)
         x = 2;

      cps = buffer;
      cpt = cptsave;
      i = 0;

      while (len > 0)
      {
         i *= 16;
         if (*cps >= '0' && *cps <= '9')
            i += (*cps - '0');
         else if (*cps >= 'A' && *cps <= 'F')
            i += (*cps - 'A' + 10);
         else if (*cps >= 'a' && *cps <= 'f')
            i += (*cps - 'a' + 10);

         cps ++;
         x --;
         len --;

         if (x <= 0)
         {
            *cpt = (unsigned char) i;
            cpt ++;
            x = 2;
            i = 0;
         }
      }

      len = (len_orig + 1) / 2;
   }
   else if (mode == 'B')
   {
      len_orig = len;

      x = len % 8;
      if (x == 0)
         x = 8;

      cps = buffer;
      cpt = cptsave;
      i = 0;

      while (len > 0)
      {
         i *= 2;
         if (*cps >= '0' && *cps <= '1')
            i += (*cps - '0');

         cps ++;
         x --;
         len --;

         if (x <= 0)
         {
            *cpt = (unsigned char) i;
            cpt ++;
            x = 8;
            i = 0;
         }
      }

      len = (len_orig + 7) / 8;
   }

   return len;
}





static int copy_set (void *vgs,
                     char *cpt,
                     char *cps)

/**********************************************************/
/*                                                        */
/*   Kopiere Set                                          */
/*                                                        */
/**********************************************************/

{
   global_store *gs = vgs;

   int len = 0;
   int x;
   unsigned short sx;
   unsigned char *cpsu;
   unsigned char *cptu;
   char c1;
   char c2;
   unsigned char bitp;
   int offs;
   int shift;
   char *pz;

   /***********************************************/
   /*   alte set-Darstellung mit integers         */
   /***********************************************/

   if (*cps == '(')
   {
      cps ++;
      while (*cps == ' ')
         cps ++;

      if (*cps == ')')
      {
         sx = 0;
         memcpy (cpt, &sx, 2);
         cpt += 2;
         len = 0;
      }
      else
      {
         for (;;)
         {
            x = atoi (cps);
            sx = x;
            memcpy (cpt, &sx, 2);
            cpt += 2;
            len += 2;

            while (*cps != ',' && *cps != ')')
               cps ++;

            if (*cps == ')')
               break;

            cps ++;
         }
      }
   }

   /***********************************************/
   /*   empty set                                 */
   /***********************************************/

   else if (*cps == 'E')
   {
      sx = 0;
      memcpy (cpt, &sx, 2);
      cpt += 2;
      len = 0;
   }

   /***********************************************/
   /*   hexadezimaler Bitstring                   */
   /***********************************************/

   else if (*cps == 'X')
   {
      cps ++;
      len = atoi (cps);
      while (*cps != '\'')
         cps ++;
      cps ++;
      for (;;)
      {
         c1 = *cps;
         if (c1 == '\'')
         {
            if (cps [1] == ',')
            {
               pz = fgets (gs -> inpzeile, 255, gs -> inpfile);
               if (pz == NULL)
                  break;

               cps = pz;
               while (*cps != '\'')
                  cps ++;
               cps ++;
               continue;
            }
            break;
         }
         cps ++;
         c2 = *cps;
         cps ++;

         sx = hexvalue (c1) * 16 + hexvalue (c2);
         cptu = (unsigned char *) cpt;
         *cptu = sx;
         cpt ++;
      }
   }

   /***********************************************/
   /*   Zeichenmenge (set of char)                */
   /***********************************************/

   else if (*cps == 'C')
   {
      cps ++;
      len = atoi (cps);
      while (*cps != '\'')
         cps ++;
      cps ++;
      memset (cpt, 0x00, len);

      for (;;)
      {
         if (*cps == '\'')
         {
            if (cps [1] == ',')
            {
               pz = fgets (gs -> inpzeile, 255, gs -> inpfile);
               if (pz == NULL)
                  break;

               cps = pz;
               while (*cps != '\'')
                  cps ++;
               cps ++;
               continue;
            }

            if (cps [1] != '\'')
               break;

            cps ++;
         }

         cpsu = (unsigned char *) cps;
         sx = *cpsu;
         offs = sx / 8;
         shift = sx % 8;

         bitp = 0x80;
         bitp >>= shift;
         cptu = (unsigned char *) cpt;
         cptu = cptu + offs;
         *cptu = *cptu | bitp;

         cps ++;
      }
   }

   return len;
}





static void conv_const (void *vgs,
                        char typ,
                        char *cp,
                        int *ivalue,
                        double *rvalue,
                        char *buffer,
                        int *len_used)

/**********************************************************/
/*                                                        */
/*   Konvertiere Konstante je nach Typ                    */
/*                                                        */
/**********************************************************/
/*                                                        */
/*   Erweiterung am 03.12.2017:                           */
/*                                                        */
/*   Typ = 0 fuer DFC 0,xx - setzt Buffer auf Hex 0       */
/*   xx ist dabei die definierte Laenge (*len_used)       */
/*                                                        */
/**********************************************************/

{
   global_store *gs = vgs;

   switch (typ)
   {
      case 'B':
      case 'H':
      case 'I':
         *ivalue = atoi (cp);
         break;

      case '0':
         *len_used = atoi (cp);
         break;

      case 'C':
         cp ++;
         *ivalue = *cp;
         break;

      case 'R':
         *rvalue = atof (cp);
         break;

      case 'M':
         *len_used = copy_string (gs, buffer, cp);
         break;

      case 'P':
         strcpy (buffer, cp);
         break;

      case 'S':
         *len_used = copy_set (gs, buffer, cp);
         break;

      default:
         break;
   }
}





static void load (void *vgs,
                  void *vpot,
                  char *plabel,
                  char *poper)

/**********************************************************/
/*                                                        */
/*   Lade Befehl und Operanden in Interpreter-            */
/*   Speicher                                             */
/*                                                        */
/**********************************************************/

{
   global_store *gs = vgs;
   opctab *pot = vpot;
   funtab *pft;
   sc_code *pcode;
   sc_code *pcode_ent;
   sc_code *pcodex;
   sc_code *pcodey;
   char *cp;
   char *cp1;
   char *cp2;
   char *cpsave;
   int *intp;
   double rvalue;
   int ivalue;
   short svalue;
   int len_used;
   int alloc_alt;
   cst_section *pcst;
   ent_section *pent;
   int len_string;
   int diff;
   int len_oper;
   char buffer [33000];

   /**********************************************************/
   /*   PCODE-Instruktion in den Speicher, vorher            */
   /*   ggf. vergroessern                                    */
   /**********************************************************/

   if (gs -> code_alloc <= gs -> code_used)
   {
      gs -> code_alloc += 10000;
      gs -> code0 = realloc (gs -> code0,
                             gs -> code_alloc
                             * sizeof (sc_code));
   }

   pcode = gs -> code0 + gs -> code_used;
   gs -> code_used ++;

   pcode -> status = 0;                  // incomplete
   pcode -> op = pot - gs -> ot;
   pcode -> t = ' ' ;
   pcode -> t2 = ' ' ;
   pcode -> p = 0;
   pcode -> q = 0;
   pcode -> x = 0;

   if (plabel != NULL)
   {
      pcode -> plabel = malloc (strlen (plabel) + 1);
      strcpy (pcode -> plabel, plabel);
   }
   else
      pcode -> plabel = NULL;

   /**********************************************************/
   /*   Operanden an PCode anhaengen                         */
   /*   vorher Kommentare mit -- wegmachen und diese         */
   /*   separat anhaengen                                    */
   /**********************************************************/

   if (poper != NULL)
   {
      int comm_vorh;

      cp1 = strstr (poper, "\'");
      cp = strstr (poper, " ; ");

      comm_vorh = (cp != NULL) && (cp1 == NULL || cp1 > cp);

      if (comm_vorh)
      {
         pcode -> pcomm = malloc (strlen (cp) + 1);
         strcpy (pcode -> pcomm, cp);
         len_oper = cp - poper;
         pcode -> poper = malloc (len_oper + 1);
         memcpy (pcode -> poper, poper, len_oper);
         pcode -> poper [len_oper] = 0x00;
      }
      else
      {
         pcode -> poper = malloc (strlen (poper) + 1);
         strcpy (pcode -> poper, poper);
         pcode -> pcomm = NULL;
      }
   }
   else
   {
      pcode -> poper = NULL;
      pcode -> pcomm = NULL;
   }

   pcode -> loc = gs -> loc_aktuell;

   //***********************************************************
   // printf ("pcode %p loc %d %s %s %s\n", pcode, pcode -> loc,
   //         (pcode -> plabel == NULL ? "" : pcode -> plabel),
   //         gs -> ot [pcode -> op] . opcode,
   //         (pcode -> poper == NULL ? "" : pcode -> poper));
   //***********************************************************

   /**********************************************************/
   /*   entsprechend Operandentyp den Operanden uebersetzen  */
   /**********************************************************/

   switch (pot -> optype)
   {
      /**********************************************************/
      /*   A = nur numerische Adresse (z.B. LOC, IXA)           */
      /**********************************************************/

      case 'A':
         pcode -> q = atoi (poper);
         break;

      /**********************************************************/
      /*   B = Level und Adresse, z.B. LDA                      */
      /**********************************************************/

      case 'B':
         cp = poper;
         do
         {
            while (*cp == ' ' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> p = atoi (cp);
            while (*cp != ',' && *cp != 0x00)
               cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> q = atoi (cp);
         }
         while (0);
         break;

      /**********************************************************/
      /*   C = Konstante, wie bei LDC                           */
      /**********************************************************/

      case 'C':
         cp = poper;
         do
         {
            while (*cp == ' ' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> t = *cp;

            if (pcode -> t == 'N')
            {
               pcode -> q = 0;
               break;
            }

            cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;

            conv_const (gs, pcode -> t,
                        cp,
                        &ivalue,
                        &rvalue,
                        buffer,
                        &len_used);

            switch (pcode -> t)
            {
               case 'B':
               case 'C':
               case 'H':
               case 'I':
                  pcode -> q = ivalue;
                  break;

               case 'R':
                  if (gs -> rconst_alloc <= gs -> rconst_used + 8)
                  {
                     gs -> rconst_alloc += 10000;
                     gs -> rconst0 = realloc (gs -> rconst0,
                                              gs -> rconst_alloc);
                  }

                  pcode -> q = gs -> rconst_used;
                  gs -> rconst_used += 8;

                  *((double *) (gs -> rconst0 + pcode -> q))
                     = rvalue;

                  break;

               default:
                  break;
            }
         }
         while (0);
         break;

      /**********************************************************/
      /*   D = Typ, Adresse (wie bei DEC und INC z.B.)          */
      /**********************************************************/

      case 'D':
         cp = poper;
         do
         {
            while (*cp == ' ' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> t = *cp;
            cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> q = atoi (cp);
         }
         while (0);
         break;

      /**********************************************************/
      /*   E = fuer LCA (Adressen von Strings usw.)             */
      /**********************************************************/

      case 'E':
         cp = poper;
         do
         {
            while (*cp == ' ' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> t = *cp;

            cp ++;
            if (*cp == ',')
               cp ++;
            if (*cp == 0x00)
               break;

            if (pcode -> t == 'M')
            {
               //*********************************************
               //*   read string
               //*   maybe max length before string
               //*   maybe X or B tag before string
               //*********************************************

               len_string = 0;

               if (*cp != '\'' && *cp != 'B' && *cp != 'X')
               {
                  len_string = atoi (cp);
                  while (*cp != ',')
                     cp ++;
                  if (*cp == ',')
                     cp ++;
                  if (*cp == 0x00)
                     break;
               }

               conv_const (gs, pcode -> t,
                           cp,
                           &ivalue,
                           &rvalue,
                           buffer,
                           &len_used);

               if (len_string > len_used)
               {
                  diff = len_string - len_used;
                  memset (buffer + len_used, ' ', diff);
                  len_used = len_string;
                  buffer [len_used] = 0x00;
               }
            }
            else
            {
               conv_const (gs, pcode -> t,
                           cp,
                           &ivalue,
                           &rvalue,
                           buffer,
                           &len_used);
            }

            switch (pcode -> t)
            {
               case 'M':

                  if (gs -> string_alloc <= gs -> string_used + 1000)
                  {
                     gs -> string_alloc += 10000;
                     gs -> string0 = realloc (gs -> string0,
                                              gs -> string_alloc);
                  }

                  pcode -> q = gs -> string_used;
                  pcode -> x = len_used;

                  memcpy (gs -> string0 + gs -> string_used,
                          buffer, len_used);

                  gs -> string_used += len_used;

                  break;

               case 'S':

                  if (gs -> string_alloc <= gs -> string_used + 1000)
                  {
                     gs -> string_alloc += 10000;
                     gs -> string0 = realloc (gs -> string0,
                                              gs -> string_alloc);
                  }

                  NHIGH (gs -> string_used, 4);

                  pcode -> q = gs -> string_used;
                  pcode -> x = len_used;

                  memcpy (gs -> string0 + gs -> string_used,
                          buffer, len_used);

                  gs -> string_used += len_used;

                  break;

               case 'P':

                  /***********************************************/
                  /*   LCA P = Laden der Adresse der             */
                  /*   statischen CSECT; Name in Label kopieren  */
                  /***********************************************/

                  pcode -> plabel = malloc (strlen (buffer) + 1);
                  strcpy (pcode -> plabel, buffer);

                  break;

               default:
                  break;
            }
         }
         while (0);
         break;

      /**********************************************************/
      /*   F = Level und Bedingung, z.B. XEN                    */
      /**********************************************************/

      case 'F':
         cp = poper;
         do
         {
            while (*cp == ' ' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> q = atoi (cp);
            while (*cp != ',' && *cp != 0x00)
               cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> p = (atoi (cp) != 0);
         }
         while (0);
         break;

      /**********************************************************/
      /*   G = Adresse und Modus (1,2,...)                      */
      /**********************************************************/

      case 'G':
         cp = poper;
         do
         {
            while (*cp == ' ' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> q = atoi (cp);
            while (*cp != ',' && *cp != 0x00)
               cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> p = atoi (cp);
         }
         while (0);
         break;

      /**********************************************************/
      /*   J = Sprungziel sichern (Operand bei FJP und UJP)     */
      /**********************************************************/

      case 'J':
         break;

      /**********************************************************/
      /*   K = Typ, Konstante (fuer DEF)                        */
      /**********************************************************/

      case 'K':
         cp = poper;
         do
         {
            while (*cp == ' ' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> t = *cp;
            cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            if (pcode -> t == 'C')
            {
               if (*cp == '\'' && *cp != 0x00)
                  cp ++;
               pcode -> q = *cp;
            }
            else
               pcode -> q = atoi (cp);
         }
         while (0);
         break;

      /**********************************************************/
      /*   L = Label (Offset uebernehmen)                       */
      /**********************************************************/

      case 'L':
         pcode -> q = gs -> code_used - 1;
         if (gs -> entry_act >= 0)
            pcode_ent = gs -> code0 + gs -> entry_act;
         else
            pcode_ent = NULL;

         if (pcode_ent == NULL ||
             memcmp (gs -> ot [pcode_ent -> op] . opcode,
                     "ENT", 3) != 0)
         {
            fprintf (stderr, "ENT not found before LAB/XLB\n");
            break;
         }

         /*******************************************/
         /*   speziell fuer XLB: Level des Labels   */
         /*******************************************/

         pent = pcode_ent -> psect;
         pcode -> p = pent -> level;
         break;

      /**********************************************************/
      /*   M = LOC (Line of Code Parameter uebernehmen)         */
      /**********************************************************/

      case 'M':
         pcode -> q = atoi (poper);
         gs -> loc_aktuell = pcode -> q;
         pcode -> loc = gs -> loc_aktuell;
         break;

      /**********************************************************/
      /*   R = RET                                              */
      /**********************************************************/

      case 'R':
         cp = poper;
         do
         {
            while (*cp == ' ' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> t = *cp;
         }
         while (0);

         if (gs -> entry_act >= 0)
            pcode_ent = gs -> code0 + gs -> entry_act;
         else
            pcode_ent = NULL;

         if (pcode_ent == NULL ||
             memcmp (gs -> ot [pcode_ent -> op] . opcode,
                     "ENT", 3) != 0)
         {
            fprintf (stderr, "ENT not found before RET\n");
            break;
         }

         /***************************************************/
         /*   fuer alle Sprungbefehle die Adressen der      */
         /*   Sprungziele einsetzen                         */
         /***************************************************/
         /*   Besonderheit bei XJP:                         */
         /*   Sprungziel L<Nummer+3> nach pcodex -> x       */
         /***************************************************/

         for (pcodex = pcode_ent;
              pcodex != pcode;
              pcodex ++)
         {
            if (pcodex -> poper != NULL)
            {
               if (memcmp (gs -> ot [pcodex -> op] . opcode,
                           "XJP", 3) == 0)
               {
                  char label1 [10];
                  char label2 [10];

                  if (memcmp (pcodex -> poper, "N,L", 3) != 0)
                  {
                     fprintf (stderr, "XJP instruction "
                              "has wrong format (not XJP N,Lx)\n");
                     break;
                  }

                  sprintf (label1, "L%d",
                           atoi (pcodex -> poper + 3));

                  sprintf (label2, "L%d",
                           atoi (pcodex -> poper + 3) + 1);

                  for (pcodey = pcode_ent;
                       pcodey != pcode;
                       pcodey ++)
                  {
                     if (pcodey -> plabel != NULL)
                     {
                        if (strcmp (label1, pcodey -> plabel) == 0)
                        {
                           pcodex -> q = pcodey - gs -> code0;
                        }

                        if (strcmp (label2, pcodey -> plabel) == 0)
                        {
                           pcodex -> x = pcodey - gs -> code0;
                        }
                     }
                  }
               }
               else if (memcmp (gs -> ot [pcodex -> op] . opcode,
                                "FJP", 3) == 0 ||
                        memcmp (gs -> ot [pcodex -> op] . opcode,
                                "UJP", 3) == 0)
               {
                  for (pcodey = pcode_ent;
                       pcodey != pcode;
                       pcodey ++)
                  {
                     if (pcodey -> plabel != NULL &&
                         *(pcodey -> plabel) != 0x00)
                     {
                        if (strcmp (pcodex -> poper,
                                    pcodey -> plabel) == 0)
                        {
                           pcodex -> q = pcodey - gs -> code0;
                        }
                     }
                  }
               }
            }
         }

         break;

      /**********************************************************/
      /*   S = Typ, Level, Adresse (wie bei STR z.B.)           */
      /**********************************************************/

      case 'S':
         cp = poper;
         do
         {
            while (*cp == ' ' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> t = *cp;
            cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> p = atoi (cp);
            while (*cp != ',' && *cp != 0x00)
               cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> q = atoi (cp);
         }
         while (0);
         break;

      /**********************************************************/
      /*   T = nur Typbuchstabe                                 */
      /**********************************************************/

      case 'T':
         cp = poper;
         do
         {
            while (*cp == ' ' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> t = *cp;
         }
         while (0);
         break;

      /**********************************************************/
      /*   U = CUP (call user procedure)                        */
      /*   Typ, Anz-Parms * 2 + 1, Name                         */
      /*   Integer-Parameter = Basisadr. fuer neues Display     */
      /**********************************************************/

      case 'U':

         if (gs -> call_mst_counter < 1)
         {
            fprintf (stderr, "counter incorrect in MST/CUP nesting\n");
            pcode -> ipmst = 0;
         }
         else
         {
            sc_code *pcode_mst;

            pcode -> ipmst =
               gs -> mst_pointer [gs -> call_mst_counter - 1];

            pcode_mst = gs -> code0 + pcode -> ipmst;

#if 0

            fprintf (stderr, "cup: %d %d unstacked %d %d\n",
                     pcode_mst -> p, pcode_mst -> q,
                     gs -> call_mst_counter,
                     gs -> code_used - 1);

#endif

            gs -> call_mst_counter --;
         }

         cp = poper;
         do
         {
            while (*cp == ' ' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> t = *cp;
            cp ++;
            if (*cp != ',')
            {
               pcode -> t2 = *cp;
               cp ++;
            }
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> p = atoi (cp);
            while (*cp != ',' && *cp != 0x00)
               cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;

            /***********************************************/
            /*   Name der Proc in das Label kopieren       */
            /***********************************************/

            pcode -> plabel = malloc (strlen (cp) + 1);
            strcpy (pcode -> plabel, cp);
            cp2 = pcode -> plabel;
            while (*cp2 != ',' && *cp2 != 0x00)
               cp2 ++;
            *cp2 = 0x00;

            /***********************************************/
            /*   einige Prozeduren, die durch den          */
            /*   Interpreter selbst implementiert          */
            /*   werden ...                                */
            /***********************************************/

            if (strcmp (pcode -> plabel, "$PASSYS ") == 0)
            {
               pcode -> q = -1;
            }

            while (*cp != ',' && *cp != 0x00)
               cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> x = atoi (cp);
         }
         while (0);
         break;

      /**********************************************************/
      /*   V = Vergleich, also Typ und bei M noch Anzahl        */
      /*   2020.12: bei M zwei Anzahlen (links, rechts)         */
      /*   wenn nur eine Anzahl vorhanden ist, dann             */
      /*   p = q setzen                                         */
      /**********************************************************/

      case 'V':
         cp = poper;
         do
         {
            while (*cp == ' ' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> t = *cp;
            cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> q = atoi (cp);
            pcode -> p = pcode -> q;
            while (*cp != ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            cp ++;
            pcode -> p = atoi (cp);
         }
         while (0);
         break;

      /**********************************************************/
      /*   W = AND, OR, XOR - logisch oder bitweise             */
      /**********************************************************/

      case 'W':
         cp = poper;
         do
         {
            while (*cp == ' ' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
            {
               pcode -> t = 'B';
               break;
            }
            pcode -> t = *cp;
         }
         while (0);
         break;

      /**********************************************************/
      /*   X = Sprungziel sichern (Operand bei XJP)             */
      /*   wird im Nachgang gemacht ...                         */
      /**********************************************************/

      case 'X':
         break;

      /**********************************************************/
      /*   Y = Call Standard Function                           */
      /**********************************************************/

      case 'Y':

         for (pft = gs -> ft;
              pft -> fucode != NULL;
              pft ++)
         {
            if (memcmp (pft -> fucode, poper, 3) == 0)
            {
               pcode -> q = pft -> cspnum;
               break;
            }
         }

         if (pft -> fucode == NULL)
         {
            fprintf (stderr,
                     "standard function %-3.3s "
                     "not found in funtab\n",
                     poper);
            break;
         }

         break;

      /**********************************************************/
      /*   Z = Typ, zwei Integers (fuer CHK)                    */
      /*   Integers koennen auch Chars sein, wenn sie mit       */
      /*   Hochkomma maskiert sind                              */
      /**********************************************************/

      case 'Z':
         cp = poper;
         do
         {
            while (*cp == ' ' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> t = *cp;
            cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            if (*cp == '\'')
            {
               cp ++;
               pcode -> p = *cp;
               cp ++;
            }
            else
            {
               pcode -> p = atoi (cp);
            }
            while (*cp != ',' && *cp != 0x00)
               cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            if (*cp == '\'')
            {
               cp ++;
               pcode -> q = *cp;
               cp ++;
            }
            else
            {
               pcode -> q = atoi (cp);
            }
         }
         while (0);
         break;

      /**********************************************************/
      /*   hat keine Operanden                                  */
      /**********************************************************/

      case '0':
         break;

      /**********************************************************/
      /*   1 = CST (sozusagen statische CSECT)                  */
      /**********************************************************/

      case '1':

         if (gs -> pcst_first == NULL)
         {
            gs -> pcst_first = malloc (sizeof (cst_section));
            gs -> pcst_last = gs -> pcst_first;
         }
         else
         {
            gs -> pcst_last -> next =
               malloc (sizeof (cst_section));
            gs -> pcst_last = gs -> pcst_last -> next;
         }

         pcst = gs -> pcst_last;
         memset (pcst, 0x00, sizeof (cst_section));
         pcst -> next = NULL;

         strcpy (pcst -> name_short, plabel);
         memcpy (pcst -> name_long, poper, IDSIZE);
         pcst -> name_long [IDSIZE] = 0x00;
         cp = pcst -> name_long + IDSIZE - 1;
         while (*cp == ' ')
         {
            *cp = 0x00;
            cp --;
         }
         pcode -> q = atoi (poper + IDSIZE);
         pcst -> lfdnr = pcode -> q;

         cp = poper + IDSIZE;

         while (*cp != ',' && *cp != 0x00)
            cp ++;
         while (*cp == ',' && *cp != 0x00)
            cp ++;
         if (*cp == 0x00)
            break;
         pcst -> flag1 = (*cp != 'F');

         while (*cp != ',' && *cp != 0x00)
            cp ++;
         while (*cp == ',' && *cp != 0x00)
            cp ++;
         if (*cp == 0x00)
            break;
         pcst -> flag2 = (*cp != 'F');

         while (*cp != ',' && *cp != 0x00)
            cp ++;
         while (*cp == ',' && *cp != 0x00)
            cp ++;
         if (*cp == 0x00)
            break;
         pcst -> flag3 = (*cp != 'F');

         break;

      /**********************************************************/
      /*   2 = DFC (Definition in statischer CSECT)             */
      /**********************************************************/

      case '2':

         pcode -> q = atoi (plabel);

         if (gs -> pcst_first == NULL)
         {
            fprintf (stderr, "CST not found before DFC\n");

            gs -> pcst_first = malloc (sizeof (cst_section));

            gs -> pcst_last = gs -> pcst_first;

            pcst = gs -> pcst_last;
            memset (pcst, INIT_PATTERN, sizeof (cst_section));
            pcst -> next = NULL;
         }

         pcst = gs -> pcst_last;

         if (pcst -> cst_alloc <= pcode -> q + 1000)
         {
            alloc_alt = pcst -> cst_alloc;
            pcst -> cst_alloc += 10000;
            pcst -> cst0 = realloc (pcst -> cst0,
                                    pcst -> cst_alloc);
            memset (pcst -> cst0 + alloc_alt, INIT_PATTERN, 10000);
         }

         cp = poper;

         do
         {
            while (*cp == ' ' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> t = *cp;

            if (pcode -> t == 'N')
            {
               intp = (int *) (pcst -> cst0 + pcode -> q);
               *intp = -1;
               pcst -> cst_used = pcode -> q + 4;
               break;
            }

            if (pcode -> t == 'M')
            {
               cp ++;
               while (*cp == ',' && *cp != 0x00)
                  cp ++;
               if (*cp == 0x00)
                  break;

               //*********************************************
               //*   read string
               //*   maybe max length before string
               //*   maybe X or B tag before string
               //*********************************************

               len_string = 0;

               if (*cp != '\'' && *cp != 'B' && *cp != 'X')
               {
                  len_string = atoi (cp);

                  if (len_string > 32000)
                  {
                     fprintf (stderr,
                              "DFC length too large - "
                              "32k is the limit\n");

                     len_string = 32000;
                  }

                  while (*cp != ',')
                     cp ++;
                  if (*cp == ',')
                     cp ++;
                  if (*cp == 0x00)
                     break;
               }

               conv_const (gs, pcode -> t,
                           cp,
                           &ivalue,
                           &rvalue,
                           buffer,
                           &len_used);

               if (len_string > len_used)
               {
                  diff = len_string - len_used;
                  memset (buffer + len_used, ' ', diff);
                  len_used = len_string;
                  buffer [len_used] = 0x00;
               }
            }
            else
            {
               cp ++;
               while (*cp == ',' && *cp != 0x00)
                  cp ++;
               if (*cp == 0x00)
                  break;

               conv_const (gs, pcode -> t,
                           cp,
                           &ivalue,
                           &rvalue,
                           buffer,
                           &len_used);
            }

            switch (pcode -> t)
            {
               case 'B':
                  pcst -> cst0 [pcode -> q] = ivalue;
                  pcst -> cst_used = pcode -> q + 1;
                  break;

               case 'C':
                  pcst -> cst0 [pcode -> q] = ivalue;
                  pcst -> cst_used = pcode -> q + 1;
                  break;

               case 'H':
                  svalue = ivalue;
                  memcpy (pcst -> cst0 + pcode -> q, &svalue, 2);
                  pcst -> cst_used = pcode -> q + 2;
                  break;

               case 'I':
                  memcpy (pcst -> cst0 + pcode -> q, &ivalue, 4);
                  pcst -> cst_used = pcode -> q + 4;
                  break;

               case 'M':
                  memcpy (pcst -> cst0 + pcode -> q, buffer, len_used);
                  pcst -> cst_used = pcode -> q + len_used;
                  break;

               case 'R':
                  memcpy (pcst -> cst0 + pcode -> q, &rvalue, 8);
                  pcst -> cst_used = pcode -> q + 8;
                  break;

               case 'S':
                  memcpy (pcst -> cst0 + pcode -> q, buffer, len_used);
                  pcst -> cst_used = pcode -> q + len_used;
                  break;

               case '0':
                  memset (pcst -> cst0 + pcode -> q, 0x00, len_used);
                  pcst -> cst_used = pcode -> q + len_used;
                  break;

               default:
                  break;
            }
         }
         while (0);

         break;

      /**********************************************************/
      /*   3 = BGN (Programmheader und Startposition)           */
      /**********************************************************/

      case '3':

         strcpy (gs -> progheader, poper);
         pcode -> q = gs -> code_used - 1;
         gs -> startpos = pcode -> q;

         break;

      /**********************************************************/
      /*   4 = ENT (Entry Point)                                */
      /**********************************************************/

      case '4':

         if (gs -> pent_first == NULL)
         {
            gs -> pent_first = malloc (sizeof (ent_section));
            gs -> pent_last = gs -> pent_first;
         }
         else
         {
            gs -> pent_last -> next =
               malloc (sizeof (ent_section));
            gs -> pent_last = gs -> pent_last -> next;
         }

         pent = gs -> pent_last;
         memset (pent, 0x00, sizeof (ent_section));
         pent -> next = NULL;

         /*******************************************/
         /*   chain code to ent section             */
         /*   backchain to code                     */
         /*******************************************/

         pcode -> q = gs -> code_used - 1;
         pcode -> psect = pent;

         gs -> entry_act = pcode -> q;

         pent -> pcodenr = pcode -> q;

         /*******************************************/
         /*   werte in pent setzen                  */
         /*******************************************/

         strcpy (pent -> name_short, plabel);

         cp = poper;

         while (*cp == ' ' && *cp != 0x00)
            cp ++;
         pcode -> t = *cp;

         // printf ("proc/func type %c ", pcode -> t);

         cp ++;
         while (*cp == ',' && *cp != 0x00)
            cp ++;
         pcode -> p = atoi (cp);
         pent -> level = pcode -> p;
         while (*cp != ',' && *cp != 0x00)
            cp ++;
         while (*cp == ',' && *cp != 0x00)
            cp ++;

         // printf ("level %d ", pcode -> p);

         memcpy (pent -> size_label, cp, 4);
         cpsave = cp + 4;
         pent -> size_label [4] = 0x00;
         cp = pent -> size_label + 3;
         while (*cp == ' ')
         {
            *cp = 0x00;
            cp --;
         }
         cp = cpsave;

         memcpy (pent -> name_long, cp, IDSIZE);
         cpsave = cp + IDSIZE;
         pent -> name_long [IDSIZE] = 0x00;
         cp = pent -> name_long + IDSIZE - 1;
         while (*cp == ' ')
         {
            *cp = 0x00;
            cp --;
         }
         cp = cpsave;

         // printf ("name %s loaded ... \n", pent -> name_long);

         //*****************************************************
         // --20220419--
         // do needed here to make sure that the
         // strcpy to pent -> sourcename is done;
         // empty sourcenames in pent prohibited debugging
         //*****************************************************

         do
         {
            while (*cp != ',' && *cp != 0x00)
               cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pent -> flag1 = (*cp != 'F');

            while (*cp != ',' && *cp != 0x00)
               cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pent -> flag2 = (*cp != 'F');

            while (*cp != ',' && *cp != 0x00)
               cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pent -> flag3 = (*cp != 'F');

            while (*cp != ',' && *cp != 0x00)
               cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pent -> flag4 = (*cp != 'F');

            while (*cp != ',' && *cp != 0x00)
               cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pent -> numb1 = atoi (cp);

            while (*cp != ',' && *cp != 0x00)
               cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pent -> numb2 = atoi (cp);
         }
         while (0);

         //*****************************************************
         // --20220419--
         // this strcpy was not done in all cases
         // before 19.04.2022 :-((
         //*****************************************************

         strcpy (pent -> sourcename, gs -> sourcename);

         break;

      /**********************************************************/
      /*   5 = Level und Adresse bei MST                        */
      /*   MST Info ausserdem stacken fuer CUP                  */
      /**********************************************************/

      case '5':
         cp = poper;
         do
         {
            while (*cp == ' ' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> p = atoi (cp);
            while (*cp != ',' && *cp != 0x00)
               cp ++;
            while (*cp == ',' && *cp != 0x00)
               cp ++;
            if (*cp == 0x00)
               break;
            pcode -> q = atoi (cp);
         }
         while (0);

         gs -> call_mst_counter ++;
         if (gs -> call_mst_counter > 16)
         {
            fprintf (stderr, "counter too large in MST/CUP nesting\n");
            gs -> call_mst_counter = 16;
         }

         gs -> mst_pointer [gs -> call_mst_counter - 1] =
            gs -> code_used - 1;

#if 0

         fprintf (stderr, "mst: %d %d stacked %d %d\n",
                  pcode -> p, pcode -> q,
                  gs -> call_mst_counter,
                  gs -> code_used - 1);

#endif

         break;

      /**********************************************************/
      /*   default: koennen wir noch nicht                      */
      /**********************************************************/

      case ' ':
         break;
   }
}





static void assemble (global_store *gs,
                      char *cplabel,
                      char *cpcode,
                      char *cpoper)

/**********************************************************/
/*                                                        */
/*   Uebersetze                                           */
/*                                                        */
/**********************************************************/

{
   opctab *pot;
   sc_code *pcode;
   int x;
   int gefunden;

   for (pot = gs -> ot; pot -> opcode != NULL; pot ++)
   {
      if (memcmp (pot -> opcode, cpcode, 3) == 0)
      {
         load (gs, pot, cplabel, cpoper);

         if (memcmp (cpcode, "XBG", 3) == 0)
         {
            gs -> code_used --;
            pcode = gs -> code0 + gs -> code_used;

#if 0
            printf ("XBG mit p = %d und q = %d\n",
                     pcode -> p, pcode -> q);
#endif

            if (gs -> xbg_xen_count >= 10)
            {
               fprintf (stderr, "XBG: xbg nesting >= 10 "
                                "not supported\n");
            }
            else
            {
               x = gs -> xbg_xen_count;
               gs -> xbg_xen_count ++;
               gs -> xbg_xen_tag [x] = pcode -> q;
               gs -> xbg_xen_codeptr [x] = gs -> code_used;
            }
         }
         else if (memcmp (cpcode, "XEN", 3) == 0)
         {
            gs -> code_used --;
            pcode = gs -> code0 + gs -> code_used;

#if 0
            printf ("XEN mit p = %d und q = %d\n",
                     pcode -> p, pcode -> q);
#endif

            gefunden = 0;

            for (x = 0; x < gs -> xbg_xen_count; x ++)
            {
               if (gs -> xbg_xen_tag [x] == pcode -> q)
               {
                  gefunden = 1;
                  break;
               }
            }

            if (! gefunden)
            {
               fprintf (stderr, "XEN: matching XBG not found\n");
            }
            else
            {
               if (! (pcode -> p))
                  gs -> code_used = gs -> xbg_xen_codeptr [x];

               gs -> xbg_xen_count --;

               for (; x < gs -> xbg_xen_count; x ++)
               {
                  gs -> xbg_xen_tag [x] =
                     gs -> xbg_xen_tag [x + 1];
                  gs -> xbg_xen_codeptr [x] =
                     gs -> xbg_xen_codeptr [x + 1];
               }
            }
         }

         break;
      }
   }

   if (pot -> opcode == NULL)
   {
      fprintf (stderr, "opcode %s not in opctab\n", cpcode);
   }
}





void translate (global_store *gs, FILE *f, char *fname)

/**********************************************************/
/*                                                        */
/*   Uebersetze                                           */
/*                                                        */
/**********************************************************/

{
   int lineno = 0;

   char label [9];
   char code [4];

   char *x;
   char *cp;
   char *cp2;
   char *pcode;
   char *poper;

   char *zeile = gs -> inpzeile;

   FILE *f_save = NULL;
   char inc_filename [100];

   gs -> entry_act = -1;

   for (;;)
   {
      x = fgets (zeile, 255, f);
      if (x == NULL)
      {
         if (f_save != NULL)
         {
            fclose (f);
            f = f_save;
            f_save = NULL;
            gs -> inpfile = f;
            continue;
         }

         break;
      }

      if (memcmp (zeile, "%INCLUDE", 8) == 0)
      {
         if (f_save != NULL)
         {
            fprintf (stderr, "nested %%INCLUDE files not allowed\n");
            break;
         }

         strcpy (inc_filename, gs -> inpfilename);
         strcat (inc_filename, ".");
         cp = inc_filename;
         cp += strlen (cp);
         cp2 = zeile + 9;

         if (strlen (cp2) <= 8)
         {
            strcpy (cp, cp2);
         }
         else
         {
            memcpy (cp, cp2, 8);
            cp [8] = 0x00;
         }

         cp += strlen (cp) - 1;

         while (*cp == ' ' || *cp == '\n' || *cp == 0x0d)
         {
            *cp = 0x00;
            cp --;
         }

         f_save = f;
         f = fopen (inc_filename, "r");
         if (f == NULL)
         {
            fprintf (stderr,
                     "%%INCLUDE file %s not found\n",
                     inc_filename);
            break;
         }

         gs -> inpfile = f;
         continue;
      }

      lineno ++;

      cp = zeile + strlen (zeile) - 1;
      if (*cp != '\n')
      {
         fprintf (stderr, "no lineend at line %d, file %s\n",
                  lineno, fname);
         break;
      }

      /**********************************************************/
      /*   Zeilenende wegmachen, d.h. Hex Null reinsetzen       */
      /**********************************************************/

      *cp = 0x00;

      /**********************************************************/
      /*   checken wg. Linux ...                                */
      /**********************************************************/

      cp = zeile + strlen (zeile) - 1;
      if (cp > zeile && *cp == 0x0d)
      {
         *cp = 0x00;
      }

      /**********************************************************/
      /*   Label aus Zeile rausholen, wenn eins da ist          */
      /**********************************************************/

      cp = zeile;
      if (*cp == ' ' || *cp == 0x00)
      {
         *label = 0x00;
      }
      else
      {
         while (*cp != ' ' && *cp != 0x00)
            cp ++;

         if (cp - zeile > 9)
         {
            fprintf (stderr, "label > 9 chars at line %d, file %s\n",
                     lineno, fname);
            break;
         }

         memcpy (label, zeile, cp - zeile);
         label [cp - zeile] = 0x00;
      }

      /**********************************************************/
      /*   Leerzeilen etc. abhandeln                            */
      /**********************************************************/

      while (*cp == ' ')
         cp ++;

      if (*cp == 0x00)
      {
         if (*label == 0x00)
         {
            continue;
         }
         else
         {
            fprintf (stderr, "label w/o opcode at line %d, file %s\n",
                     lineno, fname);
            break;
         }
      }

      /**********************************************************/
      /*   Opcode rausholen                                     */
      /**********************************************************/

      pcode = cp;
      while (*cp != ' ' && *cp != 0x00)
         cp ++;

      if (cp - pcode != 3)
      {
         fprintf (stderr, "code != 3 chars at line %d, file %s\n",
                  lineno, fname);
         break;
      }

      memcpy (code, pcode, 3);
      code [3] = 0x00;

      /**********************************************************/
      /*   Der Rest sind Operanden                              */
      /**********************************************************/

      while (*cp != ' ' && *cp != 0x00)
         cp ++;
      while (*cp == ' ' && *cp != 0x00)
         cp ++;
      poper = cp;

      gs -> local_error = 0;

      assemble (gs, label, code, poper);

      switch (gs -> local_error)
      {
         case 1:
            fprintf (stderr, "unknown type tag before string "
                             "constant, line %d, file %s\n",
                     lineno, fname);
            break;

         case 2:
            fprintf (stderr, "conversion of X type string "
                             "constant still missing, "
                             "line %d, file %s\n",
                     lineno, fname);
            break;

         case 3:
            fprintf (stderr, "conversion of B type string "
                             "constant still missing, "
                             "line %d, file %s\n",
                     lineno, fname);
            break;

         default:
            break;
      }
   }
}





void translate2 (global_store *gs)

/**********************************************************/
/*                                                        */
/*   Uebersetze Teil 2                                    */
/*                                                        */
/**********************************************************/

{
   int aktion;
   char *plabel;
   sc_code *pcode;
   sc_code *pcode2;
   cst_section *pcst;
   ent_section *pent;
   int pegel;
   char *err_opcode;

   char comp_label [9];
   char comp_label2 [9];

   /**********************************************************/
   /*                                                        */
   /*   LCA P:                                               */
   /*                                                        */
   /*   Eintragen der Adresse des CST-Elements, nachdem      */
   /*   Laenge und Offset der CST-Segmente festliegt         */
   /*                                                        */
   /*   CUP:                                                 */
   /*                                                        */
   /*   Eintragen der Adresse des ENT-Elements, damit        */
   /*   Aufruf schnell erfolgen kann                         */
   /*                                                        */
   /*   ENT:                                                 */
   /*                                                        */
   /*   ebenfalls ENT-Element eintragen                      */
   /*                                                        */
   /**********************************************************/

   for (pcode = gs -> code0;
        pcode < gs -> code0 + gs -> code_used;
        pcode ++)
   {
      aktion = 0;

      if (memcmp (gs -> ot [pcode -> op] . opcode,
                  "LCA", 3) == 0 &&
          pcode -> t == 'P')
      {
         err_opcode = "LCA";
         aktion = 1;
         plabel = pcode -> plabel;
         SETL (comp_label, plabel, 8);
      }
      else if (memcmp (gs -> ot [pcode -> op] . opcode,
                       "CUP", 3) == 0)
      {
         if (pcode -> q == 0)
         {
            err_opcode = "CUP";
            aktion = 2;
            plabel = pcode -> plabel;
            SETL (comp_label, plabel, 8);
         }
      }
      else if (memcmp (gs -> ot [pcode -> op] . opcode,
                       "UXJ", 3) == 0)
      {
         err_opcode = "UXJ";
         aktion = 3;
         plabel = pcode -> poper;
         SETL (comp_label, plabel, 8);
      }

      if (aktion == 0)
         continue;

      switch (aktion)
      {
         case 1:
            for (pcst = gs -> pcst_first;
                 pcst != NULL;
                 pcst = pcst -> next)
            {
               SETL (comp_label2, pcst -> name_short, 8);

               if (memcmp (comp_label, comp_label2, 8) == 0)
                  break;
            }
            if (pcst == NULL)
            {
               for (pent = gs -> pent_first;
                    pent != NULL;
                    pent = pent -> next)
               {
                  SETL (comp_label2, pent -> name_short, 8);

                  if (memcmp (comp_label, comp_label2, 8) == 0)
                     break;
               }
               if (pent == NULL)
               {
                  fprintf (stderr, "%s: section label %s not found\n",
                          err_opcode, plabel);
               }
               else
               {
                  pcode -> q = pent -> pcodenr;
                  pcode -> plabel = NULL;
               }
            }
            else
            {
               pcode -> psect = pcst;
               pcode -> plabel = NULL;
            }
            break;

         case 2:
            for (pent = gs -> pent_first;
                 pent != NULL;
                 pent = pent -> next)
            {
               SETL (comp_label2, pent -> name_short, 8);

               if (memcmp (comp_label, comp_label2, 8) == 0)
                  break;
            }
            if (pent == NULL)
            {
               if (memcmp (plabel, "DSIN    ", 8) == 0 ||
                   memcmp (plabel, "DCOS    ", 8) == 0 ||
                   memcmp (plabel, "DEXP    ", 8) == 0 ||
                   memcmp (plabel, "DSQRT   ", 8) == 0)
               {
#if 0
                  fprintf (stderr, "%s: section label %s not found; "
                           "FTN lib function assumed\n",
                           err_opcode, plabel);
#endif
               }
               else if (memcmp (plabel, "*PFPARM*", 8) != 0)
               {
                  fprintf (stderr, "%s: section label %s not found\n",
                           err_opcode, plabel);
               }
            }
            else
            {
               pcode -> q = pent -> pcodenr;
               pcode -> plabel = NULL;
            }
            break;

         case 3:
            for (pcode2 = pcode;
                 pcode2 < gs -> code0 + gs -> code_used;
                 pcode2 ++)
            {
               SETL (comp_label2, pcode2 -> plabel, 8);

               if (memcmp (comp_label, comp_label2, 8) == 0)
                  break;
            }
            if (pcode2 == gs -> code0 + gs -> code_used)
            {
               fprintf (stderr, "%s: long branch target %s not found\n",
                        err_opcode, plabel);
            }
            else
            {
               pcode -> q = pcode2 -> q;
               pcode -> p = pcode2 -> p;
            }
            break;

         default:
            break;
      }
   }

   /**********************************************************/
   /*                                                        */
   /*   jetzt:                                               */
   /*                                                        */
   /*   die Groessen der Konstanten-Segmente liegen fest.    */
   /*   Es gibt folgende:                                    */
   /*                                                        */
   /*   - Segmente fuer Gleitkommazahlen (rconst)            */
   /*   - Segmente fuer Strings und Sets (string)            */
   /*   - Segmente fuer const-Vektoren usw. (cst)            */
   /*                                                        */
   /*   diese werden in den oberen Bereich von STORE         */
   /*   abgelegt, ab der Adresse SCONST (siehe               */
   /*   Runtime-Parameter). Die entsprechenden               */
   /*   Instruktionen in CODE werden angeglichen,            */
   /*   also LDC (soweit betroffen) und LCA.                 */
   /*                                                        */
   /**********************************************************/
   /*                                                        */
   /*   schon jetzt klar: es wird Schwierigkeiten mit        */
   /*   den Sets geben; diese sind binaer codiert, und       */
   /*   z.B. bei SET OF CHAR entspricht die Position der     */
   /*   Bits nicht der Anordnung wie sie sein soll - bei     */
   /*   anderem Zeichensatz.                                 */
   /*                                                        */
   /*   *** gefixt 2016 ***                                  */
   /*                                                        */
   /**********************************************************/

   pegel = gs -> start_const;
   NHIGH (pegel, 8);

   gs -> rconst_start = pegel;

   pegel += gs -> rconst_used;
   NHIGH (pegel, 8);

   gs -> string_start = pegel;

   pegel += gs -> string_used;
   NHIGH (pegel, 8);

   for (pcst = gs -> pcst_first;
        pcst != NULL;
        pcst = pcst -> next)
   {
      pcst -> cst_start = pegel;

      pegel += pcst -> cst_used;
      NHIGH (pegel, 8);
   }

   for (pcode = gs -> code0;
        pcode < gs -> code0 + gs -> code_used;
        pcode ++)
   {
      if (memcmp (gs -> ot [pcode -> op] . opcode,
                  "LCA", 3) == 0)
      {
         switch (pcode -> t)
         {
            case 'M':
               pcode -> q += gs -> string_start;
               break;

            case 'S':
               pcode -> q += gs -> string_start;
               break;

            case 'P':
               if (pcode -> psect != NULL)
               {
                  pcst = pcode -> psect;
                  pcode -> q += pcst -> cst_start;
               }
               break;

            default:
               break;
         }
      }
      else if (memcmp (gs -> ot [pcode -> op] . opcode,
                       "LDC", 3) == 0)
      {
         switch (pcode -> t)
         {
            case 'R':
               pcode -> q += gs -> rconst_start;
               break;

            default:
               break;
         }
      }
   }

   //********************************************************
   // Platz fuer CMD-Line
   //********************************************************

   gs -> cmdline = pegel;
   pegel += gs -> maxsize_cmdline;


   //********************************************************
   // Platz fuer 100 File-Handles
   //********************************************************

   gs -> actfiles = 0;
   gs -> firstfilepos = pegel;
   gs -> actfilepos = pegel;
   pegel += gs -> maxfiles * sizeof (filecb);


   //********************************************************
   // Platz fuer String-Workarea
   //********************************************************

   gs -> firststring = pegel;
   gs -> actstring = pegel;
   gs -> sizestringarea = gs -> maxstring;
   pegel += gs -> sizestringarea;


   //********************************************************
   // Platz fuer new stype Heap
   //********************************************************

   gs -> firstnewheap = pegel;
   gs -> actnewheap = pegel;
   pegel += gs -> sizenewheap;

   /**********************************************************/
   /*                                                        */
   /*   STORE entsprechend anfordern und vorbelegen          */
   /*                                                        */
   /**********************************************************/

   gs -> firstalloc = pegel;
   gs -> nextalloc = pegel;

   gs -> store_alloc = pegel;
   gs -> store_used = pegel;
   gs -> store0 = malloc (pegel);

   memset (gs -> store0, INIT_PATTERN, pegel);

   gs -> stack_alloc = 4096;
   gs -> stack_used = 0;
   gs -> stack0 = malloc (4096);
   gs -> stacktype = malloc (4096);

   memset (gs -> stack0, INIT_PATTERN, 4096);
   memset (gs -> stacktype, ' ', 4096);

   memcpy (ADDRSTOR (gs -> rconst_start),
           gs -> rconst0,
           gs -> rconst_used);

   memcpy (ADDRSTOR (gs -> string_start),
           gs -> string0,
           gs -> string_used);

   for (pcst = gs -> pcst_first;
        pcst != NULL;
        pcst = pcst -> next)
   {
      memcpy (ADDRSTOR (pcst -> cst_start),
              pcst -> cst0,
              pcst -> cst_used);
   }

   free (gs -> rconst0);

   // gs -> rconst_used = 0;  // muss stehen bleiben wg. listing

   gs -> rconst_alloc = 0;
   gs -> rconst0 = NULL;

   free (gs -> string0);

   // gs -> string_used = 0;  // muss stehen bleiben wg. listing

   gs -> string_alloc = 0;
   gs -> string0 = NULL;

   /**********************************************************/
   /*                                                        */
   /*   jetzt:                                               */
   /*                                                        */
   /*   Groessenangabe bei ENT aus angegebenem Label holen   */
   /*                                                        */
   /**********************************************************/

   for (pcode = gs -> code0;
        pcode < gs -> code0 + gs -> code_used;
        pcode ++)
   {
      plabel = pcode -> plabel;

      if (memcmp (gs -> ot [pcode -> op] . opcode, "ENT", 3) == 0)
      {
         pent = pcode -> psect;
         if (pent == NULL)
            continue;

         for (pcode2 = pcode + 1;
              pcode2 < gs -> code0 + gs -> code_used;
              pcode2 ++)
         {
            if (memcmp (gs -> ot [pcode2 -> op] . opcode, "ENT", 3)
                == 0)
               break;

            if (memcmp (gs -> ot [pcode2 -> op] . opcode, "DEF", 3)
                != 0)
               continue;

            if (strcmp (pcode2 -> plabel, pent -> size_label) == 0)
            {
               pcode -> x = pcode2 -> q;
               pent -> size = pcode2 -> q;
            }
         }
      }
   }
}





static void dumparea (FILE *outfile,
                      char *header,
                      char *origin,
                      char *cpx,
                      int len,
                      int align)

{
   unsigned int w;
   char *cpstart, *cpstop;
   char *cp;
   unsigned int i;

   static char *zulzeichen = "0123456789"
                             "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                             "abcdefghijklmnopqrstuvwxyz"
                             " !\"$%&/()=?+*#\'-_.:,;<>\\"
                             "{[]}~^";

   if (header != NULL)
   {
      fprintf (outfile, "\n%s\n\n", header);
   }

   w = cpx - origin;
   if (align)
   {
      w &= ~ 0xFUL;
   }
   cpstart = origin + w;

   w = cpx + len - origin;
   if (align)
   {
      w += 0xFUL;
      w &= ~ 0xFUL;
   }
   cpstop = origin + w;

   for (cp = cpstart; cp < cpstop; cp += 16)
   {
      fprintf (outfile, "%08d ", (int) (cp - origin));
      fprintf (outfile, "%08x: ", (int) (cp - origin));
      for (i = 0; i < 16; i++)
      {
         if ((i & 0x3) == 0)
            fprintf (outfile, " ");
         if (cp + i >= cpx && cp + i < cpx + len)
            fprintf (outfile, "%02X", (unsigned char) (*(cp + i)));
         else
            fprintf (outfile, "  ");
      }
      fprintf (outfile, "  +");
      for (i = 0; i < 16; i++)
      {
         if (cp + i >= cpx && cp + i < cpx + len)
            if (strchr (zulzeichen, *(cp + i)) != NULL
                && *(cp + i) != '\0')
               fprintf (outfile, "%c", *(cp + i));
            else
               fprintf (outfile, ".");
         else
            fprintf (outfile, " ");
      }
      fprintf (outfile, "+\n");
   }
}




void dump_stack (FILE *outfile,
                 char *header,
                 char *origin,
                 int start,
                 int len,
                 int align)

/**********************************************************/
/*                                                        */
/*   Ausgeben eines Ausschnitts von STACK                 */
/*                                                        */
/**********************************************************/

{
   if (start < 0)
   {
      len += start;
      start = 0;
   }

   dumparea (outfile,
             header,
             origin,
             origin + start,
             len,
             align);
}




void dump_store (FILE *outfile,
                 char *header,
                 char *origin,
                 int start,
                 int len,
                 int align)

/**********************************************************/
/*                                                        */
/*   Ausgeben eines Ausschnitts von STORE                 */
/*                                                        */
/**********************************************************/

{
   if (start < 0)
   {
      len += start;
      start = 0;
   }

   dumparea (outfile,
             header,
             origin,
             origin + start,
             len,
             align);
}





void read_pascal (global_store *gs, char *pasfilename)

{
   FILE *pasfile;
   char *cp;
   void *pcache;
   int rc;
   char zeile [132];

   source_key sk;
   source_data sd;

   source_key *psk;
   source_data *psd;
   int len1 = sizeof (source_key);
   int len2 = sizeof (source_data);

   char *cachename = "SOURCE";

   //*******************************************************
   //*   hier wichtig, damit Source-Cache korrekt gefuellt wird
   //*******************************************************

   if (strlen (pasfilename) > sizeof (gs -> sourcename) - 1)
   {
      memcpy (gs -> sourcename,
              pasfilename,
              sizeof (gs -> sourcename) - 1);
   }
   else
   {
      strcpy (gs -> sourcename, pasfilename);
   }

   pasfile = fopen (pasfilename, "r");

   if (pasfile == NULL)
      return;

   cp = pasfilename + strlen (pasfilename);
   while (cp >= pasfilename && *cp != '/' && *cp != '\\')
      cp --;
   if (*cp == '/' || *cp == '\\')
      cp ++;

   memset (&sk, 0x00, sizeof (sk));

   strcpy (sk.sourcename, gs -> sourcename);

   //*******************************************************
   //*   Source-Cache anlegen, falls noch nicht geschehen
   //*******************************************************

   if (gs -> sourcecache == NULL)
   {
      len1 = 6;
      len2 = 4;

      rc = oiscach ("CREATE", NULL, NULL,
                    &cachename, &len1, (char **) &pcache, &len2);
      if (rc != 0)
      {
         printf ("Fehler %d bei oiscach-create\n", rc);
         exit (20);
      }

      gs -> sourcecache = pcache;
   }

   sk.loc = 0;

   for (;;)
   {
      if (fgets (zeile, 128, pasfile) == NULL)
         break;

      sk.loc += 1;

      cp = zeile + strlen (zeile);
      cp --;
      if (*cp == '\n')
         *cp = 0x00;

      strcpy (sd.zeile, zeile);

      psk = &sk;
      psd = &sd;
      len1 = sizeof (source_key);
      len2 = sizeof (source_data);

#if 0
      printf ("put cache: %s %d %s\n",
               sk.sourcename, sk.loc, sd.zeile);
#endif

      rc = oiscach ("PUT", gs -> sourcecache, NULL,
                    (char **) &psk, &len1,
                    (char **) &psd, &len2);
   }
}





void listing (global_store *gs)

{
   char *plabel;
   char *poper;
   char *pcomm;
   sc_code *pcode;
   char header [100];
   cst_section *pcst;
   ent_section *pent;
   char outmst [100];

   fprintf (gs -> outfile, "PCODE-Assembler-Listing\n\n");

   /**********************************************************/
   /*                                                        */
   /*   Ausgeben der P-Code Instruktionen                    */
   /*   inklusive interner Darstellung                       */
   /*                                                        */
   /**********************************************************/

   for (pcode = gs -> code0;
        pcode < gs -> code0 + gs -> code_used;
        pcode ++)
   {
      plabel = pcode -> plabel;
      poper = pcode -> poper;
      pcomm = pcode -> pcomm;

      if (memcmp (gs -> ot [pcode -> op] . opcode, "ENT", 3) == 0)
      {
         pent = pcode -> psect;
         fprintf (gs -> outfile,
                  "\nSourcefile = %s\n", pent -> sourcename);
         strcpy (gs -> sourcename, pent -> sourcename);
      }

      if (memcmp (gs -> ot [pcode -> op] . opcode, "ENT", 3) == 0 ||
          memcmp (gs -> ot [pcode -> op] . opcode, "CST", 3) == 0)
      {
         fprintf (gs -> outfile,
                  "\nOffset  OpN T AddrP     AddrQ     AddrX   "
                  "Label    Opc Operands\n\n");
      }

      *outmst = 0x00;

      if (pcode -> ipmst != 0)
      {
         sc_code *pcode_mst = gs -> code0 + pcode -> ipmst;

         sprintf (outmst, " MST at %06d %d %d",
                 (int) (pcode_mst - gs -> code0),
                 pcode_mst -> p,
                 pcode_mst -> q);
      }

      fprintf (gs -> outfile,
               "%06d: %03d %c%c %4d %9d %9d   %-8s %-3s %s%s %s\n",
               (int) (pcode - gs -> code0),
               pcode -> op,
               pcode -> t,
               pcode -> t2,
               pcode -> p,
               pcode -> q,
               pcode -> x,
               (plabel != NULL ? plabel : ""),
               gs -> ot [pcode -> op] . opcode,
               (poper != NULL ? poper : ""),
               (pcomm != NULL ? pcomm : ""),
               outmst);

      /**********************************************************/
      /*   Pascal-Source-Zeilen anhaengen, sofern vorhanden     */
      /**********************************************************/

      if (gs -> sourcecache != NULL)
      {
         int rc;
         source_key sk;
         source_key *psk;
         int len1 = sizeof (source_key);
         int len2 = sizeof (source_data);
         source_data *psd;

         if (memcmp (gs -> ot [pcode -> op] . opcode,
                     "LOC", 3) == 0 ||
             memcmp (gs -> ot [pcode -> op] . opcode,
                     "ENT", 3) == 0)
         {
            memset (&sk, 0x00, sizeof (sk));
            strcpy (sk.sourcename, gs -> sourcename);
            sk.loc = pcode -> loc;
            psk = &sk;

            rc = oiscach ("GET", gs -> sourcecache, NULL,
                           (char **) &psk, &len1,
                           (char **) &psd, &len2);

            if (rc == 0)
            {
               fprintf (gs -> outfile,
                        "*** LOC %4d: %s\n", sk.loc, psd -> zeile);

            //    printf ("get cache: %s %d %s\n",
            //             sk.sourcename, sk.loc, psd -> zeile);
            }
            else
            {
            //    printf ("get cache: %s %d %s\n",
            //             sk.sourcename, sk.loc, "++ nix gefunden!");
            }

            pcode -> psource = psd -> zeile;
         }
      }
   }

   /**********************************************************/
   /*                                                        */
   /*   das Listing muss auch die statischen Bereiche        */
   /*   ausgeben, mit entsprechender Kommentierung           */
   /*                                                        */
   /**********************************************************/

   dump_store (gs -> outfile,
               "ConstArea RCONST",
               gs -> store0,
               gs -> rconst_start,
               gs -> rconst_used,
               1);

   dump_store (gs -> outfile,
               "ConstArea STRING",
               gs -> store0,
               gs -> string_start,
               gs -> string_used,
               1);

   for (pcst = gs -> pcst_first;
        pcst != NULL;
        pcst = pcst -> next)
   {
      sprintf (header, "ConstArea %s", pcst -> name_short);

      dump_store (gs -> outfile,
                  header,
                  gs -> store0,
                  pcst -> cst_start,
                  pcst -> cst_used,
                  1);
   }

   /**********************************************************/
   /*                                                        */
   /*   Ausgabe der ENT-Sections                             */
   /*                                                        */
   /**********************************************************/

   fprintf (gs -> outfile, "\n\nTabelle der ENT Sections\n\n");

   fprintf (gs -> outfile,
            "%-8.8s     %c %s %-20.20s %.6s %-8.8s %.5s\n\n",
            "Kurzname", 'T', "Lev", "langer Name",
            " PCode", "SizeLbl", " Size");

   for (pent = gs -> pent_first;
        pent != NULL;
        pent = pent -> next)
   {
      pcode = gs -> code0 + pent -> pcodenr;

      fprintf (gs -> outfile,
               "%-8.8s ENT %c %3d %-20.20s %6d %-8.8s %5d "
               "%d %d %d %d %5d %5d\n",
               pent -> name_short,
               pcode -> t,
               pcode -> p,
               pent -> name_long,
               pent -> pcodenr,
               pent -> size_label,
               pent -> size,
               pent -> flag1,
               pent -> flag2,
               pent -> flag3,
               pent -> flag4,
               pent -> numb1,
               pent -> numb2);
   }

   fprintf (gs -> outfile, "\n");
}





