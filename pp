cp $1.prr $1.prralt
env DD_INPUT=$1.pas DD_LISTING=$1.lis DD_PCODE=$1.prr DD_DBGINFO=$1.dbginfo DD_PRD=pascal.messages DD_TRACEF=*stdout* pcint prr=pascal1.prr inc=paslibx,passcan pas=pascal1.pas out=pascal1.prrlis debug=n
