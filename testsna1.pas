program TEST1 ( OUTPUT ) ;

/********/
/*$N+,A+*/
/********/



type CHARSET = packed array [ CHAR ] of CHAR ;
     MNEM_TABLE = array [ 0 .. 255 ] of array [ 1 .. 4 ] of CHAR ;
     INTPTR = -> INTEGER ;

     (*****************************************************************)
     (*                                                               *)
     (*    WIR TESTEN HIER MAL EINEN GROESSEREN KOMMENTAR             *)
     (*    DER UEBER MEHRERE ZEILEN GEHT ..................           *)
     (*                                                               *)
     (*****************************************************************)



var CH : CHAR ;
    X : CHARSET ;
    F : packed array [ 1 .. 6 ] of CHAR ;
    I : INTEGER ;
    ZTBLN : MNEM_TABLE ;


const Y : array [ 1 .. 5 ] of INTEGER =
      ( 4 , 5 , 7 , 9 , 12 ) ;
      XTBLN : MNEM_TABLE =
      ( '(00)' , '(01)' , '(02)' , '(03)' , 'SPM ' , 'BALR' , 'BCTR' ,
        'BCR ' , 'SSK ' , 'ISK ' , 'SVC ' , '(0B)' , '(0C)' , '(0D)' ,
        'MVCL' , 'CLCL' , 'LPR ' , 'LNR ' , 'LTR ' , 'LCR ' , 'NR  ' ,
        'CLR ' , 'OR  ' , 'XR  ' , 'LR  ' , 'CR  ' , 'AR  ' , 'SR  ' ,
        'MR  ' , 'DR  ' , 'ALR ' , 'SLR ' , 'LPDR' , 'LNDR' , 'LTDR' ,
        'LCDR' , 'HDR ' , 'LRDR' , 'MXR ' , 'MXDR' , 'LDR ' , 'CDR ' ,
        'ADR ' , 'SDR ' , 'MDR ' , 'DDR ' , 'AWR ' , 'SWR ' , 'LPER' ,
        'LNER' , 'LTER' , 'LCER' , 'HER ' , 'LRER' , 'AXR ' , 'SXR ' ,
        'LER ' , 'CER ' , 'AER ' , 'SER ' , 'MER ' , 'DER ' , 'AUR ' ,
        'SUR ' , 'STH ' , 'LA  ' , 'STC ' , 'IC  ' , 'EX  ' , 'BAL ' ,
        'BCT ' , 'BC  ' , 'LH  ' , 'CH  ' , 'AH  ' , 'SH  ' , 'MH  ' ,
        '(4D)' , 'CVD ' , 'CVB ' , 'ST  ' , '(51)' , '(52)' , '(53)' ,
        'N   ' , 'CL  ' , 'O   ' , 'X   ' , 'L   ' , 'C   ' , 'A   ' ,
        'S   ' , 'M   ' , 'D   ' , 'AL  ' , 'SL  ' , 'STD ' , '(61)' ,
        '(62)' , '(63)' , '(64)' , '(65)' , '(66)' , 'MXD ' , 'LD  ' ,
        'CD  ' , 'AD  ' , 'SD  ' , 'MD  ' , 'DD  ' , 'AW  ' , 'SW  ' ,
        'STE ' , '(71)' , '(72)' , '(73)' , '(74)' , '(75)' , '(76)' ,
        '(77)' , 'LE  ' , 'CE  ' , 'AE  ' , 'SE  ' , 'ME  ' , 'DE  ' ,
        'AU  ' , 'SU  ' , 'SSM ' , '(81)' , 'LPSW' , 'DIAG' , 'WRD ' ,
        'RDD ' , 'BXH ' , 'BXLE' , 'SRL ' , 'SLL ' , 'SRA ' , 'SLA ' ,
        'SRDL' , 'SLDL' , 'SRDA' , 'SLDA' , 'STM ' , 'TM  ' , 'MVI ' ,
        'TS  ' , 'NI  ' , 'CLI ' , 'OI  ' , 'XI  ' , 'LM  ' , '(99)' ,
        '(9A)' , '(9B)' , 'SIO ' , 'TIO ' , 'HIO ' , 'TCH ' , '(A0)' ,
        '(A1)' , '(A2)' , '(A3)' , '(A4)' , '(A5)' , '(A6)' , '(A7)' ,
        '(A8)' , '(A9)' , '(AA)' , '(AB)' , '(AC)' , '(AD)' , '(AE)' ,
        '(AF)' , '(B0)' , 'LRA ' , 'STCK' , '(B3)' , '(B4)' , '(B5)' ,
        'STCT' , 'LCTL' , '(B8)' , '(B9)' , '(BA)' , '(BB)' , '(BC)' ,
        'CLM ' , 'STCM' , 'ICM ' , '(C0)' , '(C1)' , '(C2)' , '(C3)' ,
        '(C4)' , '(C5)' , '(C6)' , '(C7)' , '(C8)' , '(C9)' , '(CA)' ,
        '(CB)' , '(CC)' , '(CD)' , '(CE)' , '(CF)' , '(D0)' , 'MVN ' ,
        'MVC ' , 'MVZ ' , 'NC  ' , 'CLC ' , 'OC  ' , 'XC  ' , '(D8)' ,
        '(D9)' , '(DA)' , '(DB)' , 'TR  ' , 'TRT ' , 'ED  ' , 'EDMK' ,
        '(E0)' , '(E1)' , '(E2)' , '(E3)' , '(E4)' , '(E5)' , '(E6)' ,
        '(E7)' , '(E8)' , '(E9)' , '(EA)' , '(EB)' , '(EC)' , '(ED)' ,
        '(EE)' , '(EF)' , 'SRP ' , 'MVO ' , 'PACK' , 'UNPK' , '(F4)' ,
        '(F5)' , '(F6)' , '(F7)' , 'ZAP ' , 'CP  ' , 'AP  ' , 'SP  ' ,
        'MP  ' , 'DP  ' , '(FE)' , '(FF)' ) ;



procedure CHARSET_INI ( var X : CHARSET ) ;

   var CH : CHAR ;

   begin (* CHARSET_INI *)

     (********************************************)
     (* HIER SIND NEUERDINGS KOMMENTARE MOEGLICH *)
     (********************************************)

     for CH := CHR ( 0 ) to CHR ( 255 ) do
       X [ CH ] := ' '
   end (* CHARSET_INI *) ;



procedure CHARSET_ADD ( var X : CHARSET ; VON : CHAR ; BIS : CHAR ) ;

   var CH : CHAR ;

   begin (* CHARSET_ADD *)
     for CH := VON to BIS do
       begin
         if CH = 'F' then
           return ;
         X [ CH ] := 'J'
       end (* for *)
   end (* CHARSET_ADD *) ;



function IN_CHARSET ( var X : CHARSET ; SUCH : CHAR ) : BOOLEAN ;

   begin (* IN_CHARSET *)
     SNAPSHOT ( 5 , 10 ) ;
     IN_CHARSET := ( X [ SUCH ] <> ' ' ) ;
   end (* IN_CHARSET *) ;



function IN_CHARSET1 ( var X : CHARSET ; SUCH : CHAR ; ZUSATZ : CHAR )
                     : BOOLEAN ;

   begin (* IN_CHARSET1 *)
     IN_CHARSET1 := ( X [ SUCH ] <> ' ' ) or ( SUCH = ZUSATZ ) ;
   end (* IN_CHARSET1 *) ;



begin (* HAUPTPROGRAMM *)

  (***********************************)
  (* DIESER KOMM SOLL STEHEN BLEIBEN *)
  (***********************************)

  WRITELN ( 'DATE=' , DATE ) ;

  /*****************************************/
  /* CONTINUE; /* -- YIELDS ERROR E71 -- */*/
  /*****************************************/

  ZTBLN := XTBLN ;
  F := 'BERND ' ;
  for I := 1 to 6 do
    begin
      if F [ I ] = 'D' then
        break ;
      if F [ I ] = 'E' then
        continue ;
      WRITELN ( 'FOR-SCHLEIFE: I = ' , I : 3 , ' F(I) = ' , F [ I ] ) ;
    end (* for *) ;
  WRITELN ( 'I NACH DER SCHLEIFE =' , I : 5 ) ;
  I := 1 ;
  while F [ I ] <> ' ' do
    begin
      if F [ I ] = 'D' then
        break ;
      if F [ I ] = 'R' then
        begin
          I := I + 1 ;
          continue
        end (* then *) ;
      WRITELN ( 'WHILE-SCHLEIFE: I = ' , I : 3 ) ;
      I := I + 1 ;
    end (* while *) ;
  WRITELN ( 'I NACH DER SCHLEIFE =' , I : 5 ) ;
  I := 1 ;
  repeat
    if F [ I ] = 'D' then
      break ;
    if F [ I ] = 'R' then
      begin
        I := I + 1 ;
        continue
      end (* then *) ;
    WRITELN ( 'REPEAT-SCHLEIFE: I = ' , I : 3 ) ;
    I := I + 1 ;
  until F [ I ] = ' ' ;
  WRITELN ( 'I NACH DER SCHLEIFE =' , I : 5 ) ;
  F := 'BERND ' ;
  for I := 1 to 6 do
    begin
      WRITELN ( 'FOR-SCHLEIFE: I = ' , I : 3 , ' F(I) = ' , F [ I ] ) ;
      break ;

  (*****************************)
  (* DIESER KOMM BLEIBT STEHEN *)
  (*****************************)

    end (* for *) ;
  WRITELN ( 'I NACH DER SCHLEIFE =' , I : 5 ) ;
  I := 1 ;
  while F [ I ] <> ' ' do
    begin
      WRITELN ( 'WHILE-SCHLEIFE: I = ' , I : 3 ) ;
      I := I + 1 ;
      break ;
    end (* while *) ;
  WRITELN ( 'I NACH DER SCHLEIFE =' , I : 5 ) ;
  I := 1 ;
  repeat
    WRITELN ( 'REPEAT-SCHLEIFE: I = ' , I : 3 ) ;
    I := I + 1 ;
    break ;
  until F [ I ] = ' ' ;
  WRITELN ( 'I NACH DER SCHLEIFE =' , I : 5 ) ;
  F := 'BERND ' ;
  for I := 1 to 6 do
    begin
      WRITELN ( 'FOR-SCHLEIFE: I = ' , I : 3 , ' F(I) = ' , F [ I ] ) ;
      continue ;
    end (* for *) ;
  WRITELN ( 'I NACH DER SCHLEIFE =' , I : 5 ) ;
  I := 1 ;
  while F [ I ] <> ' ' do
    begin
      WRITELN ( 'WHILE-SCHLEIFE: I = ' , I : 3 ) ;
      I := I + 1 ;
      continue ;
    end (* while *) ;
  WRITELN ( 'I NACH DER SCHLEIFE =' , I : 5 ) ;
  I := 1 ;
  repeat
    WRITELN ( 'REPEAT-SCHLEIFE: I = ' , I : 3 ) ;
    I := I + 1 ;
    continue ;
  until F [ I ] = ' ' ;
  WRITELN ( 'I NACH DER SCHLEIFE =' , I : 5 ) ;
  CHARSET_INI ( X ) ;
  CHARSET_ADD ( X , 'A' , 'Z' ) ;
  CH := 'A' ;
  WRITELN ( 'ORD VON A = ' , ORD ( CH ) ) ;
  WRITELN ( 'AUSGABE VON X:' ) ;
  WRITELN ( X ) ;
  WRITELN ( 'ENDE AUSGABE VON X' ) ;
  WRITELN ( F ) ;

  /*********/
  /*RETURN;*/
  /*********/

  WRITELN ( 'BUCHSTABE A IN X: ' , IN_CHARSET ( X , 'A' ) ) ;
  WRITELN ( 'ZIFFER 5 IN X: ' , IN_CHARSET ( X , '5' ) ) ;

  (*************************)
  (* KOMMENTAR ZUM SCHLUSS *)
  (*************************)

  for I := 0 to 255 do
    WRITELN ( I , ' = ' , XTBLN [ I ] ) ;
end (* HAUPTPROGRAMM *) .
