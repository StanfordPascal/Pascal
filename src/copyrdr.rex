/* REXX */

/************************************************/
/*                                              */
/*   Kopieren File nach VM-Reader               */
/*                                              */
/************************************************/

arg pcfile vmuser vmname vmtype

if pcfile = "" | vmuser = "" then do
   say "usage: copyrdr <pcfile> <vmuser> <vmname> <vmtype>"
   exit
end

userid = left(vmuser, 8)

if vmname <> "" then
   vmname = left(vmname, 8)
else
   vmname = "READFILE"

if vmtype <> "" then
   vmtype = left(vmtype, 8)
else
   vmtype = "DATA    "

zeile1 = "USERID "userid" CLASS A NAME "vmname" "vmtype
zeile2 = ":READ  "vmname" "vmtype" A1"

fname = "copyrdr.tmp"
x = lineout(fname, zeile1)
x = lineout(fname, zeile2)

exit

