/* REXX */

/************************************************/
/*                                              */
/*   Kopieren File nach VM-Reader               */
/*                                              */
/************************************************/

arg pcfile vmuser

if pcfile = "" | vmuser = "" then do
   say "usage: copyr <pcfile> <vmuser>"
   exit
end

userid = left(vmuser, 8)

x = pos(".",pcfile)
if x = 0 then do
   vmname = pcfile
   vmtype = "DATA"
end
else do
   vmname = left(pcfile, x - 1)
   vmtype = substr(pcfile, x + 1)
end
vmname = left(vmname, 8)
vmtype = left(vmtype, 8)

zeile1 = "USERID "userid" CLASS A NAME "vmname" "vmtype
zeile2 = ":READ  "vmname" "vmtype" A1"

fname = "copyr.tmp"
x = lineout(fname, zeile1)
x = lineout(fname, zeile2)

exit

