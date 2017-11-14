
#ifndef __OISCACH_H__

#define __OISCACH_H__




/**********************************************************/
/*                                                        */
/*   OISCACH.H                                            */
/*                                                        */
/*   Cache implementieren durch AVL-Baeume                */
/*                                                        */
/*   Autor: Bernd Oppolzer                                */
/*          2014                                          */
/*                                                        */
/**********************************************************/
/*                                                        */
/*   funkcode = CREATE:                                   */
/*   Cache anlegen, Name = *pkey (8 Stellen)              */
/*   Rueckgabe Handle ueber pdat                          */
/*                                                        */
/*   funkcode = GET:                                      */
/*   Cache-Handle mitgeben                                */
/*   Eintrag mit pkey, lkey suchen,                       */
/*   Rueckgabe ueber pdat und ldat                        */
/*                                                        */
/*   funkcode = PUT:                                      */
/*   Cache-Handle mitgeben                                */
/*   Eintrag mit pkey, lkey suchen,                       */
/*   mit Daten bei pdat bzw. ldat neu einfuegen           */
/*   bzw. ersetzen                                        */
/*                                                        */
/*   funkcode = GFIRST:                                   */
/*   Cache-Handle mitgeben                                */
/*   ersten Eintrag suchen                                */
/*   Rueckgabe Key ueber pkey und lkey und                */
/*   Daten ueber pdat und ldat                            */
/*   Leseposition anhand seqkey                           */
/*                                                        */
/*   funkcode = GNEXT:                                    */
/*   Cache-Handle mitgeben                                */
/*   naechsten Eintrag suchen                             */
/*   Rueckgabe Key ueber pkey und lkey und                */
/*   Daten ueber pdat und ldat                            */
/*   Leseposition anhand seqkey                           */
/*                                                        */
/*   funkcode = TRACE:                                    */
/*   Cache-Handle mitgeben                                */
/*   Cache ausdrucken                                     */
/*                                                        */
/*   funkcode = DELETE:                                   */
/*   Cache-Handle mitgeben                                */
/*   Cache komplett loeschen                              */
/*                                                        */
/**********************************************************/

int oiscach (char *funkcode,   // Funktionscode
             void *phandle,    // Zeiger auf Cachehandle
             void **seqkey,    // Zeiger fuer seq. Keyposition,
             char **pkey,      // Zeiger auf Zeiger auf Key
             int  *lkey,       // Zeiger auf Laenge Key
             char **pdat,      // Zeiger auf Zeiger auf Daten
             int  *ldat);      // Zeiger auf Laenge Daten




#endif

