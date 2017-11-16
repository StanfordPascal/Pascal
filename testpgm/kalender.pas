program KALENDER ( INPUT , OUTPUT , DRUCKER ) ;


type MONATE = ( JAN , FEB , MAR , APR , MAI , JUN , JUL , AUG , SEP ,
              OKT , NOV , DEZ ) ;
     JAHR = array [ MONATE ] of INTEGER ;
     TSTRING = packed array [ 1 .. 24 ] of CHAR ;
     WOCHE = array [ 1 .. 7 ] of record
                                   TAG : INTEGER ;
                                   MONAT : INTEGER ;
                                   FEIERTAG : BOOLEAN
                                 end ;
     KALFELD = array [ 1 .. 70 ] of WOCHE ;
     WOFELD = array [ 1 .. 70 ] of INTEGER ;


var DRUCKER : TEXT ;
    BEZUGSJAHR , OSTERTAG : INTEGER ;
    SCHALTTAG : INTEGER ;
    MONATSLAENGE : JAHR ;
    MONATSWERT : MONATE ;
    SONNTAG : INTEGER ;
    KAL : KALFELD ;
    WO : WOFELD ;
    LAUFTAG : INTEGER ;
    NRWOCHE : INTEGER ;



procedure VORBESETZEN ;

   var W , T : INTEGER ;

   begin (* VORBESETZEN *)
     BEZUGSJAHR := 0 ;
     for W := 1 to 70 do
       for T := 1 to 7 do
         begin
           KAL [ W ] [ T ] . TAG := 0 ;
           KAL [ W ] [ T ] . MONAT := 0 ;
           KAL [ W ] [ T ] . FEIERTAG := FALSE
         end (* for *) ;
     for W := 1 to 70 do
       WO [ W ] := 0 ;
     MONATSLAENGE [ JAN ] := 31 ;
     MONATSLAENGE [ FEB ] := 28 ;
     MONATSLAENGE [ MAR ] := 31 ;
     MONATSLAENGE [ APR ] := 30 ;
     MONATSLAENGE [ MAI ] := 31 ;
     MONATSLAENGE [ JUN ] := 30 ;
     MONATSLAENGE [ JUL ] := 31 ;
     MONATSLAENGE [ AUG ] := 31 ;
     MONATSLAENGE [ SEP ] := 30 ;
     MONATSLAENGE [ OKT ] := 31 ;
     MONATSLAENGE [ NOV ] := 30 ;
     MONATSLAENGE [ DEZ ] := 31 ;
   end (* VORBESETZEN *) ;



procedure MONATSNAME ( M : MONATE ) ;

   begin (* MONATSNAME *)
     case M of
       JAN : WRITELN ( ' JANUAR' ) ;
       FEB : WRITELN ( ' FEBRUAR' ) ;
       MAR : WRITELN ( ' MAERZ' ) ;
       APR : WRITELN ( ' APRIL' ) ;
       MAI : WRITELN ( ' MAI' ) ;
       JUN : WRITELN ( ' JUNI' ) ;
       JUL : WRITELN ( ' JULI' ) ;
       AUG : WRITELN ( ' AUGUST' ) ;
       SEP : WRITELN ( ' SEPTEMBER' ) ;
       OKT : WRITELN ( ' OKTOBER' ) ;
       NOV : WRITELN ( ' NOVEMBER' ) ;
       DEZ : WRITELN ( ' DEZEMBER' ) ;
     end (* case *) ;
   end (* MONATSNAME *) ;



procedure MONATSNAMEDRU ( M : INTEGER ) ;

   begin (* MONATSNAMEDRU *)
     case M of
       1 : WRITE ( DRUCKER , ' Jan   ' ) ;
       2 : WRITE ( DRUCKER , ' Feb   ' ) ;
       3 : WRITE ( DRUCKER , ' M„r   ' ) ;
       4 : WRITE ( DRUCKER , ' Apr   ' ) ;
       5 : WRITE ( DRUCKER , ' Mai   ' ) ;
       6 : WRITE ( DRUCKER , ' Jun   ' ) ;
       7 : WRITE ( DRUCKER , ' Jul   ' ) ;
       8 : WRITE ( DRUCKER , ' Aug   ' ) ;
       9 : WRITE ( DRUCKER , ' Sep   ' ) ;
       10 : WRITE ( DRUCKER , ' Okt   ' ) ;
       11 : WRITE ( DRUCKER , ' Nov   ' ) ;
       12 : WRITE ( DRUCKER , ' Dez   ' ) ;
     end (* case *) ;
   end (* MONATSNAMEDRU *) ;



procedure WOCHENTAG ( T : INTEGER ) ;

   begin (* WOCHENTAG *)
     case T of
       0 : WRITE ( '   SONNTAG,' ) ;
       1 : WRITE ( '    MONTAG,' ) ;
       2 : WRITE ( '  DIENSTAG,' ) ;
       3 : WRITE ( '  MITTWOCH,' ) ;
       4 : WRITE ( 'DONNERSTAG,' ) ;
       5 : WRITE ( '   FREITAG,' ) ;
       6 : WRITE ( '   SAMSTAG,' ) ;
     end (* case *) ;
   end (* WOCHENTAG *) ;



procedure OSTERN ;

   var A , B , C , D , E : INTEGER ;
       DH , EH : INTEGER ;

   begin (* OSTERN *)
     if ( BEZUGSJAHR >= 1583 ) and ( BEZUGSJAHR <= 1699 ) then
       DH := 22 ;
     if ( BEZUGSJAHR >= 1700 ) and ( BEZUGSJAHR <= 1899 ) then
       DH := 23 ;
     if ( BEZUGSJAHR >= 1900 ) and ( BEZUGSJAHR <= 2199 ) then
       DH := 24 ;
     if ( BEZUGSJAHR >= 2200 ) and ( BEZUGSJAHR <= 2299 ) then
       DH := 25 ;
     if ( BEZUGSJAHR >= 1583 ) and ( BEZUGSJAHR <= 1699 ) then
       EH := 2 ;
     if ( BEZUGSJAHR >= 1700 ) and ( BEZUGSJAHR <= 1799 ) then
       EH := 3 ;
     if ( BEZUGSJAHR >= 1800 ) and ( BEZUGSJAHR <= 1899 ) then
       EH := 4 ;
     if ( BEZUGSJAHR >= 1900 ) and ( BEZUGSJAHR <= 2099 ) then
       EH := 5 ;
     if ( BEZUGSJAHR >= 2100 ) and ( BEZUGSJAHR <= 2199 ) then
       EH := 6 ;
     if ( BEZUGSJAHR >= 2200 ) and ( BEZUGSJAHR <= 2299 ) then
       EH := 0 ;
     A := BEZUGSJAHR MOD 19 ;
     B := BEZUGSJAHR MOD 4 ;
     C := BEZUGSJAHR MOD 7 ;
     D := ( 19 * A + DH ) MOD 30 ;
     E := ( 2 * B + 4 * C + 6 * D + EH ) MOD 7 ;
     OSTERTAG := D + E ;
     if OSTERTAG = 35 then
       OSTERTAG := 28 ;
     if ( ( D = 28 ) and ( E = 6 ) and ( A > 10 ) ) then
       OSTERTAG := 27 ;
     if ( ( B = 0 ) and ( ( BEZUGSJAHR MOD 100 <> 0 ) or ( BEZUGSJAHR
     MOD 400 = 0 ) ) ) then
       SCHALTTAG := 1
     else
       SCHALTTAG := 0 ;
     OSTERTAG := OSTERTAG + SCHALTTAG + 81 ;
   end (* OSTERN *) ;



procedure UMRECHNUNG ( TAG : INTEGER ; NAME : TSTRING ) ;

   var WERKTAG : INTEGER ;
       T : INTEGER ;

   begin (* UMRECHNUNG *)
     WERKTAG := TAG ;
     MONATSWERT := JAN ;
     while TAG > MONATSLAENGE [ MONATSWERT ] do
       begin
         TAG := TAG - MONATSLAENGE [ MONATSWERT ] ;
         MONATSWERT := SUCC ( MONATSWERT ) ;
       end (* while *) ;
     T := ( WERKTAG + 7 - SONNTAG ) MOD 7 ;
     if T = 0 then
       T := 7 ;
     if TAG = 1 then
       NRWOCHE := NRWOCHE + 1
     else
       if T = 1 then
         NRWOCHE := NRWOCHE + 1 ;
     KAL [ NRWOCHE ] [ T ] . TAG := TAG ;
     KAL [ NRWOCHE ] [ T ] . MONAT := ORD ( MONATSWERT ) + 1 ;
     if T in [ 6 , 7 ] then
       KAL [ NRWOCHE ] [ T ] . FEIERTAG := TRUE
   end (* UMRECHNUNG *) ;



procedure MARKFEIERTAG ( TAG : INTEGER ; NAME : TSTRING ) ;

   var WERKTAG : INTEGER ;
       W , T : INTEGER ;

   begin (* MARKFEIERTAG *)
     WERKTAG := TAG ;
     MONATSWERT := JAN ;
     while TAG > MONATSLAENGE [ MONATSWERT ] do
       begin
         TAG := TAG - MONATSLAENGE [ MONATSWERT ] ;
         MONATSWERT := SUCC ( MONATSWERT ) ;
       end (* while *) ;
     WRITE ( ' ' , NAME ) ;
     WOCHENTAG ( ( WERKTAG + 7 - SONNTAG ) MOD 7 ) ;
     WRITE ( ' ' , TAG : 2 , '.' ) ;
     MONATSNAME ( MONATSWERT ) ;
     for W := 1 to 70 do
       for T := 1 to 7 do
         begin
           if ( KAL [ W ] [ T ] . TAG = TAG ) and ( KAL [ W ] [ T ] .
           MONAT = ORD ( MONATSWERT ) + 1 ) then
             KAL [ W ] [ T ] . FEIERTAG := TRUE
         end (* for *) ;
   end (* MARKFEIERTAG *) ;



procedure WOFUELL ;

   var W : INTEGER ;

   begin (* WOFUELL *)
     if KAL [ 1 ] [ 5 ] . TAG > 1 then
       WO [ 1 ] := 1
     else
       WO [ 1 ] := 0 ;
     for W := 2 to 70 do
       begin
         if KAL [ W ] [ 1 ] . TAG <> 0 then
           WO [ W ] := WO [ W - 1 ] + 1
         else
           WO [ W ] := WO [ W - 1 ]
       end (* for *)
   end (* WOFUELL *) ;



procedure DUMPKAL ;

   var W , T : INTEGER ;

   begin (* DUMPKAL *)
     for W := 1 to 70 do
       begin
         for T := 1 to 7 do
           begin
             WRITE ( KAL [ W ] [ T ] . TAG : 2 ) ;
             WRITE ( '.' ) ;
             WRITE ( KAL [ W ] [ T ] . MONAT : 2 ) ;
             if KAL [ W ] [ T ] . FEIERTAG then
               WRITE ( '*  ' )
             else
               WRITE ( '   ' ) ;
           end (* for *) ;
         WRITELN
       end (* for *)
   end (* DUMPKAL *) ;



procedure DRUCKKAL ;

   var W , T : INTEGER ;
       MONATALT : INTEGER ;

   begin (* DRUCKKAL *)
     MONATALT := 99 ;
     REWRITE ( DRUCKER ) ;
     WRITELN ( DRUCKER , '.S 0' ) ;
     WRITELN ( DRUCKER , '.Z 0' ) ;
     WRITELN ( DRUCKER , '.E 4' ) ;
     WRITELN ( DRUCKER , '.L 2' ) ;
     WRITELN ( DRUCKER , '.P' ) ;
     WRITE ( DRUCKER , BEZUGSJAHR : 5 ) ;
     WRITELN ( DRUCKER , ' ' : 12 , 'Mo  Di  Mi  Do  Fr  Sa  So' ) ;
     WRITELN ( DRUCKER ) ;
     for W := 1 to 70 do
       begin
         if KAL [ W ] [ 1 ] . MONAT <> MONATALT then
           begin
             WRITELN ( DRUCKER , '.L 2' ) ;
             MONATALT := KAL [ W + 1 ] [ 1 ] . MONAT ;
             MONATSNAMEDRU ( MONATALT )
           end (* then *)
         else
           begin
             if W < 70 then
               if KAL [ W + 1 ] [ 1 ] . MONAT <> MONATALT then
                 WRITELN ( DRUCKER , '.L 3' ) ;
             WRITE ( DRUCKER , ' ' : 7 ) ;
           end (* else *) ;
         WRITE ( DRUCKER , ' Wo' , WO [ W ] : 3 , ' ' : 4 ) ;
         for T := 1 to 7 do
           begin
             if KAL [ W ] [ T ] . TAG <> 0 then
               WRITE ( DRUCKER , KAL [ W ] [ T ] . TAG : 2 )
             else
               WRITE ( DRUCKER , ' ' : 2 ) ;
             if KAL [ W ] [ T ] . FEIERTAG then
               WRITE ( DRUCKER , '* ' )
             else
               WRITE ( DRUCKER , '  ' ) ;
           end (* for *) ;
         WRITELN ( DRUCKER )
       end (* for *)
   end (* DRUCKKAL *) ;



begin (* HAUPTPROGRAMM *)
  VORBESETZEN ;
  while ( BEZUGSJAHR < 1584 ) or ( BEZUGSJAHR > 2299 ) do
    begin
      if ( BEZUGSJAHR > 0 ) then
        WRITE ( ' UNGUELTIGES BEZUGSJAHR ' , BEZUGSJAHR : 1 , '.' ) ;
      WRITELN ( ' GIB DAS BEZUGSJAHR AN ' ) ;
      READ ( BEZUGSJAHR ) ;
      BEZUGSJAHR := ABS ( BEZUGSJAHR ) ;
    end (* while *) ;
  WRITELN ( ' DIE BERECHNUNG FUER DAS BEZUGSJAHR ' , BEZUGSJAHR : 4 ,
            ' LAUTET' ) ;
  OSTERN ;
  if SCHALTTAG = 1 then
    MONATSLAENGE [ FEB ] := 29 ;
  SONNTAG := OSTERTAG MOD 7 ;
  NRWOCHE := 0 ;
  for LAUFTAG := 1 to 365 + SCHALTTAG do
    UMRECHNUNG ( LAUFTAG , '                        ' ) ;
  MARKFEIERTAG ( 1 , 'NEUJAHR                 ' ) ;
  MARKFEIERTAG ( 6 , 'HL. DREI KOENIGE        ' ) ;
  MARKFEIERTAG ( OSTERTAG - 48 , 'ROSENMONTAG             ' ) ;
  MARKFEIERTAG ( OSTERTAG - 2 , 'KARFREITAG              ' ) ;
  MARKFEIERTAG ( OSTERTAG , 'OSTERSONNTAG            ' ) ;
  MARKFEIERTAG ( OSTERTAG + 1 , 'OSTERMONTAG             ' ) ;
  MARKFEIERTAG ( 121 + SCHALTTAG , '1. MAI                  ' ) ;
  MARKFEIERTAG ( OSTERTAG + 39 , 'CHRISTI HIMMELFAHRT     ' ) ;
  MARKFEIERTAG ( OSTERTAG + 49 , 'PFINGSTSONNTAG          ' ) ;
  MARKFEIERTAG ( OSTERTAG + 50 , 'PFINGSTMONTAG           ' ) ;
  MARKFEIERTAG ( OSTERTAG + 60 , 'FRONLEICHNAM            ' ) ;
  MARKFEIERTAG ( 276 + SCHALTTAG , '3. OKTOBER              ' ) ;
  MARKFEIERTAG ( 305 + SCHALTTAG , 'ALLERHEILIGEN           ' ) ;
  MARKFEIERTAG ( 359 + SCHALTTAG , '1. WEIHNACHTSFEIERTAG   ' ) ;
  MARKFEIERTAG ( 360 + SCHALTTAG , '2. WEIHNACHTSFEIERTAG   ' ) ;
  WOFUELL ;
  DRUCKKAL
end (* HAUPTPROGRAMM *) .
