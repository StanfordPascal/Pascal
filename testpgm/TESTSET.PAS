program TESTSET ( OUTPUT ) ;

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
    C : CHAR ;
    XSET : set of FARBE ;
    CSET : set of CHAR ;



function MAJOR ( CH : CHAR ) : CHAR ;

   begin (* MAJOR *)
     if CH in [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' ] then
       MAJOR := CHR ( ORD ( CH ) - ORD ( 'a' ) + ORD ( 'A' ) )
     else
       MAJOR := CH
   end (* MAJOR *) ;



function MINOR ( CH : CHAR ) : CHAR ;

   begin (* MINOR *)
     if CH in [ 'A' .. 'I' , 'J' .. 'R' , 'S' .. 'Z' ] then
       MINOR := CHR ( ORD ( CH ) - ORD ( 'A' ) + ORD ( 'a' ) )
     else
       MINOR := CH
   end (* MINOR *) ;



begin (* HAUPTPROGRAMM *)
  XSET := [ ] ;
  CSET := [ ] ;
  XSET := [ ROT , GRUEN ] ;
  CSET := [ 'B' , 'E' , 'R' , 'N' , 'D' ] ;
  for C := 'A' to 'Z' do
    if C in CSET then
      CSET := CSET + [ MINOR ( C ) ] ;
  CSET := CSET + [ 'o' , 'p' , 'p' , 'o' , 'l' , 'z' , 'e' , 'r' ] ;
  for C := 'a' to 'z' do
    if C in CSET then
      CSET := CSET + [ MAJOR ( C ) ] ;
  for C := CHR ( 0 ) to CHR ( 255 ) do
    if C in CSET then
      WRITE ( C ) ;
  WRITELN ;
  for X := GELB to BLAU do
    if X in XSET then
      WRITELN ( X ) ;
end (* HAUPTPROGRAMM *) .
