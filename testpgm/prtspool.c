#include <stdio.h>
#include <string.h>

/******************************************************************/
/* prtspool by Tim Pinkawa (http://timpinkawa.net/hercules)       */
/* Written in December 2006, released in June 2007                */
/******************************************************************/

int main (int argc, char **argv)

{
   int job = 1;
   int endCount = 0;
   char line [200];
   char ss [200];
   char jobEnd [15];
   char cmd [225];
   int i;
   FILE* jobfp;
   char path [250];
   int endOfJob = 0;

   //*****************************************************
   //* print usage etc.
   //*****************************************************

   if (argc != 3)
   {
      printf ("prtspool - a simple print spooler "
              "for emulated 1403 printers\n");
      printf ("By Tim Pinkawa "
              "(http://www.timpinkawa.net/hercules)\n\n");
      printf ("usage: %s {msgclass} {output_directory}\n",
              argv [0]);
      return 0;
   }

   printf ("*** Start PRTSPOOL ***\n");

   sprintf (jobEnd, "****%s   END", argv [1]);

   //*****************************************************
   //* read pipe
   //*****************************************************

   while (! feof (stdin))
   {
      fgets (line, 200, stdin);
      if (strcmp (line, "\f") == 0)
         break;

      for (i = 1; i < 200; i++)
         ss [i - 1] = line [i];

      //*****************************************************
      //* build filename for output file
      //* read until end of listing found
      //*****************************************************

      sprintf (path, "%s%s%04d%s",
               argv [2], "job_", job, ".txt");
      jobfp = fopen (path, "w");

      printf ("*** PRTSPOOL oeffnet Datei %s ***\n", path);

      fprintf (jobfp, ss);
      endCount = 0;
      endOfJob = 0;
      while (! endOfJob && ! feof (stdin))
      {
         fgets (line, 200, stdin);
         if (strstr (line, jobEnd) != NULL)
            endCount++;
         if (endCount == 4)
            endOfJob = 1;
         fprintf (jobfp, line);
      }

      fclose (jobfp);

      printf ("*** PRTSPOOL schliesst Datei %s ***\n", path);

      //*****************************************************
      //* next file
      //*****************************************************

      job ++;
   }

   return 0;
}

