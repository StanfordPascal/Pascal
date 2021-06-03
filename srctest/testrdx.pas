program TESTRDX ( INPUT , OUTPUT ) ;

(********)
(*$A+   *)
(********)



type FARBE = ( GELB , ROT , GRUEN , BLAU ) ;
     OPTYPE = ( PCTS , PCTI , PLOD , PSTR , PLDA , PLOC , PSTO , PLDC ,
              PLAB , PIND , PINC , PPOP , PCUP , PENT , PRET , PCSP ,
              PIXA , PEQU , PNEQ , PGEQ , PGRT , PLEQ , PLES , PUJP ,
              PFJP , PXJP , PCHK , PNEW , PADI , PADR , PSBI , PSBR ,
              PSCL , PFLT , PFLO , PTRC , PNGI , PNGR , PSQI , PSQR ,
              PABI , PABR , PNOT , PAND , PIOR , PDIF , PINT , PUNI ,
              PINN , PMOD , PODD , PMPI , PMPR , PDVI , PDVR , PMOV ,
              PLCA , PDEC , PSTP , PSAV , PRST , PCHR , PORD , PDEF ,
              PRND , PCRD , PXPO , PBGN , PEND , PASE , PSLD , PSMV ,
              PMST , PUXJ , PXLB , PCST , PDFC , PPAK , PADA , PSBA ,
              UNDEF_OP ) ;


var I : INTEGER ;
    b : boolean ;
    F : FARBE ;
    OP : OPTYPE ;


begin (* HAUPTPROGRAMM *)
  WRITELN ( 'bitte Integer eingeben:' ) ;
  READLN ( I : 5 ); ;
  WRITELN ( I ) ;
  WRITELN ( 'bitte Boolean eingeben:' ) ;
  READLN ( b:2 ) ;
  WRITELN ( b ) ;
  WRITELN ( 'bitte Farbe eingeben:' ) ;
  READLN ( F ) ;
  WRITELN ( F ) ;
  WRITELN ( 'bitte Opcode eingeben (P + 3stellig):' ) ;
  READLN ( OP ) ;
  WRITELN ( OP ) ;
end (* HAUPTPROGRAMM *) .
