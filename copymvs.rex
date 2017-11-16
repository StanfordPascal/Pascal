/* REXX */

/************************************************/
/*                                              */
/*   Kopieren File nach MVS Dataset             */
/*   mit IEBGENER-Job                           */
/*                                              */
/************************************************/

arg pcfile mvsfile

if pcfile = "" | mvsfile = "" then do
   say "usage: copymvs <pcfile> <mvsfile>"
   exit
end

x = pos(".",pcfile)
if x = 0 then do
   membname = pcfile
end
else do
   membname = left(pcfile, x - 1)
end

zeile1 = "//PASCALN1 JOB (ACCNT),'IEBGENER',CLASS=A,MSGCLASS=X,"
zeile2 = "//            USER=PASCALN,PASSWORD=PAS"
zeile3 = "//*"
zeile4 = "//* INPUT DATA INTO A MEMBER WITH IEBGENER"
zeile5 = "//*"
zeile6 = "//INPUT    EXEC PGM=IEBGENER"
zeile7 = "//SYSPRINT DD  DUMMY"
zeile8 = "//SYSIN    DD  *"
zeile9 = "//SYSUT2   DD  DISP=SHR,DSN=PASCALN."mvsfile"("membname")"
zeilea = "//SYSUT1   DD  DATA,DLM='$$'"

zeileb = "$$"

fname = "copymvs.tmp"
x = lineout(fname, zeile1)
x = lineout(fname, zeile2)
x = lineout(fname, zeile3)
x = lineout(fname, zeile4)
x = lineout(fname, zeile5)
x = lineout(fname, zeile6)
x = lineout(fname, zeile7)
x = lineout(fname, zeile8)
x = lineout(fname, zeile9)
x = lineout(fname, zeilea)

fname2 = "copymvs2.tmp"
x = lineout(fname2, zeileb)

exit

