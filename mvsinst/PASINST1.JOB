//PASCALNI JOB (PASCAL),'PASCAL INST1',CLASS=A,MSGCLASS=X,
//             TIME=1440,REGION=0M,MSGLEVEL=(1,1),
//             USER=PASCALN,PASSWORD=PAS
//*
//********************************************************************
//* link SPLITMVS utility
//* needed to SPLIT transfer file into individual members
//* distributed as object file on RUNTIME.TEXT
//********************************************************************
//*
//LNKSPLIT EXEC PGM=IEWLF880
//SYSLIB   DD  DISP=SHR,DSN=PASCALN.RUNTIME.TEXT
//SYSLMOD  DD  DISP=SHR,DSN=PASCALN.TESTPGM.LOAD
//SYSPRINT DD  SYSOUT=*
//SYSUT1   DD  UNIT=SYSDA,SPACE=(CYL,(1,1))
//SYSLIN   DD  DDNAME=SYSIN
//SYSIN    DD  *
  INCLUDE SYSLIB(SPLITMVS)
  INCLUDE SYSLIB(PASMONN)
  INCLUDE SYSLIB(PASLIBX)
  INCLUDE SYSLIB(PASUTILS)
  NAME SPLITMVS(R)
//*
