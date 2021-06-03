#!/usr/bin/rexx

/************************************************/
/*                                              */
/*   Pascal Compiler starten                    */
/*                                              */
/************************************************/

parse arg filename

env = ""
z = "DD:INPUT="filename".pas"
env = env" "z
z = "DD:OUTPUT="filename".lis"
env = env" "z
z = "DD:PRR="filename".prr"
env = env" "z
z = "DD:DBGINFO="filename".dbginfo"
env = env" "z
z = "DD:PRD=pascal.messages"
env = env" "z

"env "env" pcint prr=pascal1.prr pas=pascal1.pas out=pascal1.prrlis debug=N"

