program TESTSCAL ( INPUT , OUTPUT ) ;

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


var X : FARBE ;
    OPC : OPTYPE ;


begin (* HAUPTPROGRAMM *)
  for X := ROT to GRUEN do
    begin
      WRITELN ( 'Farbe: ' , ORD ( X ) , ' ' , '<' , PRED ( X ) : 10 ,
                '#' , X : 10 , '#' , SUCC ( X ) : 10 , '>' ) ;
    end (* for *) ;
  WRITELN ( 'liste aller opcodes' ) ;
  for OPC := PCTS to UNDEF_OP do
    begin
      WRITE ( OPC : 4 , ' ' ) ;
      if ORD ( OPC ) MOD 10 = 9 then
        WRITELN
    end (* for *) ;
end (* HAUPTPROGRAMM *) .
