program LISTGENC ( OUTPUT , MUSTER , PROGRAMM , PROTOKOL ) ;


const LINESIZE = 180 ;
      MAXLEER = 10 ;


type ZEILTYP = packed array [ 1 .. LINESIZE ] of CHAR ;


var MUSTER : TEXT ;
    PROGRAMM : TEXT ;
    PROTOKOL : TEXT ;
    ZEILE : ZEILTYP ;
    CHARANZ : INTEGER ;



function ISTELLEN ( I : INTEGER ) : INTEGER ;

   begin (* ISTELLEN *)
     if I <= 9 then
       ISTELLEN := 1
     else
       if I <= 99 then
         ISTELLEN := 2
       else
         if I <= 999 then
           ISTELLEN := 3
         else
           if I <= 9999 then
             ISTELLEN := 4
           else
             ISTELLEN := 5
   end (* ISTELLEN *) ;



procedure ZEILREIN ;

   var CH : CHAR ;
       FERTIG : BOOLEAN ;
       DUMMY : CHAR ;

   begin (* ZEILREIN *)
     CHARANZ := 0 ;
     while not EOLN ( MUSTER ) do
       begin
         READ ( MUSTER , CH ) ;
         CHARANZ := CHARANZ + 1 ;
         if CHARANZ <= LINESIZE then
           ZEILE [ CHARANZ ] := CH
       end (* while *) ;
     READLN ( MUSTER ) ;
     if CHARANZ > LINESIZE then
       CHARANZ := LINESIZE ;
     FERTIG := CHARANZ < 1 ;
     while not FERTIG do
       begin
         FERTIG := ZEILE [ CHARANZ ] <> ' ' ;
         if not FERTIG then
           begin
             CHARANZ := CHARANZ - 1 ;
             FERTIG := CHARANZ < 1
           end (* then *)
       end (* while *)
   end (* ZEILREIN *) ;



procedure INFORAUS ( X , Y : INTEGER ) ;

   var I : INTEGER ;
       LASTBLANK : INTEGER ;
       HILF : INTEGER ;
       HILF2 : INTEGER ;


   function PLATZ ( X , Y : INTEGER ) : INTEGER ;

      var HILF : INTEGER ;
          I : INTEGER ;

      begin (* PLATZ *)
        HILF := 0 ;
        for I := X to Y do
          if not ( ZEILE [ I ] in [ '''' , '"' , '\' ] ) then
            HILF := HILF + 1
          else
            HILF := HILF + 2 ;
        PLATZ := HILF
      end (* PLATZ *) ;


   begin (* INFORAUS *)
     repeat
       HILF := PLATZ ( X , Y ) ;
       HILF2 := Y - X + 1 ;
       if HILF <= 40 then
         begin
           WRITE ( PROGRAMM , '   fprintf (fp, "%-' , HILF2 : ISTELLEN
                   ( HILF2 ) , 's", "' ) ;
           for I := X to Y do
             if not ( ZEILE [ I ] in [ '''' , '"' , '\' ] ) then
               WRITE ( PROGRAMM , ZEILE [ I ] )
             else
               WRITE ( PROGRAMM , '\' , ZEILE [ I ] ) ;
           WRITELN ( PROGRAMM , '"); ' ) ;
           X := Y + 1
         end (* then *)
       else
         begin
           LASTBLANK := 0 ;
           I := X ;
           HILF := 0 ;
           repeat
             if not ( ZEILE [ I ] in [ '''' , '"' , '\' ] ) then
               HILF := HILF + 1
             else
               HILF := HILF + 2 ;
             if ZEILE [ I ] = ' ' then
               LASTBLANK := I ;
             I := I + 1
           until HILF > 35 ;
           if LASTBLANK = 0 then
             LASTBLANK := I - 1 ;
           HILF := PLATZ ( X , LASTBLANK ) ;
           HILF2 := LASTBLANK - X + 1 ;
           WRITE ( PROGRAMM , '   fprintf (fp, "%-' , HILF2 : ISTELLEN
                   ( HILF2 ) , 's", "' ) ;
           for I := X to LASTBLANK do
             if not ( ZEILE [ I ] in [ '''' , '"' , '\' ] ) then
               WRITE ( PROGRAMM , ZEILE [ I ] )
             else
               WRITE ( PROGRAMM , '\' , ZEILE [ I ] ) ;
           WRITELN ( PROGRAMM , '"); ' ) ;
           X := LASTBLANK + 1 ;
         end (* else *)
     until X > Y
   end (* INFORAUS *) ;



procedure BEFEHLERAUS ;

   var NONBLANK : INTEGER ;
       START : INTEGER ;
       FERTIG : BOOLEAN ;
       ANZBLANK : INTEGER ;
       LAUF : INTEGER ;
       I : INTEGER ;

   begin (* BEFEHLERAUS *)
     if CHARANZ > 0 then
       begin
         NONBLANK := 1 ;
         while ZEILE [ NONBLANK ] = ' ' do
           NONBLANK := NONBLANK + 1 ;
         if NONBLANK > 1 then
           if NONBLANK = 2 then
             WRITELN ( PROGRAMM , '   fprintf (fp, " ");' )
           else
             WRITELN ( PROGRAMM , '   fprintf (fp, "%' , ( NONBLANK - 1
                       ) : ISTELLEN ( NONBLANK - 1 ) , 's", "");' ) ;
         START := NONBLANK ;
         repeat
           FERTIG := FALSE ;
           LAUF := START ;
           ANZBLANK := 0 ;
           while not FERTIG do
             begin
               if LAUF > CHARANZ then
                 FERTIG := TRUE
               else
                 begin
                   if ZEILE [ LAUF ] = ' ' then
                     ANZBLANK := ANZBLANK + 1
                   else
                     ANZBLANK := 0 ;
                   FERTIG := ANZBLANK >= MAXLEER ;
                   if not FERTIG then
                     LAUF := LAUF + 1
                 end (* else *)
             end (* while *) ;
           if LAUF > CHARANZ then
             begin
               INFORAUS ( START , CHARANZ ) ;
               WRITELN ( PROGRAMM , '   fprintf (fp, "\n");' ) ;
               START := LAUF ;
             end (* then *)
           else
             INFORAUS ( START , LAUF - ANZBLANK ) ;
           if LAUF <= CHARANZ then
             begin
               LAUF := LAUF - ANZBLANK + 1 ;
               START := LAUF ;
               repeat
                 LAUF := LAUF + 1
               until ZEILE [ LAUF ] <> ' ' ;
               WRITELN ( PROGRAMM , '   fprintf (fp, "%' , ( LAUF -
                         START ) : ISTELLEN ( LAUF - START ) ,
                         's", "");' ) ;
               START := LAUF ;
             end (* then *) ;
         until START > CHARANZ
       end (* then *)
     else
       WRITELN ( PROGRAMM , '   fprintf (fp, "\n");' ) ;
     WRITELN ( PROGRAMM ) ;
   end (* BEFEHLERAUS *) ;



procedure TESTDUMP ;

   var I : INTEGER ;

   begin (* TESTDUMP *)
     WRITELN ( PROTOKOL , 'Neue Zeile - Char-Anzahl = ' , CHARANZ : 3 )
               ;
     for I := 1 to CHARANZ do
       WRITELN ( PROTOKOL , I : 3 , '-' , ZEILE [ I ] , '-' ) ;
   end (* TESTDUMP *) ;



begin (* HAUPTPROGRAMM *)
  RESET ( MUSTER ) ;
  REWRITE ( PROGRAMM ) ;
  REWRITE ( PROTOKOL ) ;
  WRITELN ( PROGRAMM , 'void print (FILE *fp)' ) ;
  WRITELN ( PROGRAMM ) ;
  WRITELN ( PROGRAMM , '{' ) ;
  repeat
    ZEILREIN ;
    BEFEHLERAUS
  until EOF ( MUSTER ) ;
  WRITELN ( PROGRAMM , '}' ) ;
end (* HAUPTPROGRAMM *) .
