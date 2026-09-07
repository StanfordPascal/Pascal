program AUFBLOG ( LOGFILE , OUTPUT , XMLOUT ) ;

//***********************************************************
// compilefehler, wenn const bei work-meldung (string-parm)  
// vermutlich VLD zu lang ...                                
//***********************************************************



var LOGFILE : TEXT ;
    XMLOUT : TEXT ;
    ZEILE : CHAR ( 150 ) ;
    DATUM : CHAR ( 10 ) ;
    UHRZEIT : CHAR ( 8 ) ;

    //********************************************************
    // Felder, in die die aktuelle Nachricht eingelesen wird  
    //********************************************************

    MELD_GESAMT : STRING ( 20000 ) ;
    MELD_GESAMT_HEX : STRING ( 10000 ) ;
    MELD_DATUM : CHAR ( 10 ) ;
    MELD_UHRZEIT : CHAR ( 8 ) ;
    MELD_ECHTLAENGE : INTEGER ;

    //****************
    // andere Felder  
    //****************

    MELDUNG : CHAR ( 80 ) ;
    ECHTLAENGE : STRING ( 80 ) ;
    MELD1 : STRING ( 80 ) ;
    MELD2 : STRING ( 80 ) ;
    LAENGE_HEADER : INTEGER ;
    POS_UMLAUTE : array [ 1 .. 1000 ] of INTEGER ;
    ANZ_UML : INTEGER ;
    OFFS_MELDUNG : INTEGER ;
    I : INTEGER ;
    X : INTEGER ;



procedure OUTXMLCHAR ( var F : TEXT ; CH : CHAR ; const CH2 : STRING )
                     ;

   begin (* OUTXMLCHAR *)
     if CH2 = '25' then
       begin
         WRITELN ( F ) ;
         return
       end (* then *) ;
     if CH = '.' then
       begin
         if ( CH2 <> '0D' ) and ( CH2 <> '00' ) then
           WRITE ( F , CH )
       end (* then *)
     else
       WRITE ( F , CH )
   end (* OUTXMLCHAR *) ;



procedure WORK_MELDUNG ( var OUTFILE : TEXT ;                   //
                       const MELD1 : STRING ;                   //
                       const MELD2 : STRING ;                   //
                       var LAENGE_HEADER : INTEGER ) ;          //

   var T1 : STRING ( 100 ) ;
       T2 : STRING ( 100 ) ;
       X : INTEGER ;
       SUCH_HEADER : INTEGER ;

   begin (* WORK_MELDUNG *)
     if LENGTH ( MELD1 ) = 0 then
       return ;
     X := 0 ;
     LAENGE_HEADER := 0 ;
     SUCH_HEADER := LENGTH ( MELD2 ) - 8 ;
     if SUCH_HEADER > 1024 then
       SUCH_HEADER := 1024 ;
     while X < SUCH_HEADER do
       begin
         T1 := SUBSTR ( MELD2 , X + 1 , 8 ) ;
         if ( T1 = '0D250D25' ) then
           begin
             LAENGE_HEADER := ( X + 7 ) DIV 2 ;
             break
           end (* then *) ;
         X := X + 2 ;
       end (* while *) ;
     WRITELN ( XMLOUT , 'Laenge Header = ' , LAENGE_HEADER ) ;
     WRITELN ( OUTFILE ) ;
     X := 0 ;
     while X < LENGTH ( MELD1 ) do
       begin
         T1 := SUBSTR ( MELD1 , X + 1 , 80 ) ;
         T2 := SUBSTR ( MELD2 , X + 1 , 80 ) ;
         WRITELN ( OUTFILE , X DIV 2 : - 5 , ': ' , T1 ) ;
         WRITELN ( OUTFILE , ' ' : 7 , T2 ) ;
         X := X + 80 ;
       end (* while *) ;
     WRITELN ( OUTFILE ) ;
     X := 0 ;
     while X < LENGTH ( MELD1 ) do
       begin
         if X < LENGTH ( MELD1 ) - 4 then
           T1 := SUBSTR ( MELD1 , X + 1 , 4 )
         else
           T1 := ' ' ;
         T2 := SUBSTR ( MELD2 , X + 1 , 2 ) ;
         if T1 = '> < ' then
           begin
             OUTXMLCHAR ( OUTFILE , '>' , '' ) ;
             WRITELN ( OUTFILE ) ;
           end (* then *)
         else
           OUTXMLCHAR ( OUTFILE , T1 [ 1 ] , T2 ) ;
         X := X + 2 ;
       end (* while *) ;
     WRITELN ( XMLOUT ) ;
     WRITELN ( XMLOUT ) ;
     if ANZ_UML = 0 then
       WRITELN ( XMLOUT , '*** keine Umlaute *** ' )
     else
       for I := 1 to ANZ_UML do
         WRITELN ( XMLOUT , 'Umlaut bei ' , POS_UMLAUTE [ I ] : 1 ,
                   ' / ' , POS_UMLAUTE [ I ] - LAENGE_HEADER : 1 ) ;
     WRITELN ( XMLOUT ) ;
     WRITELN ( XMLOUT ) ;
     WRITELN ( XMLOUT ) ;
   end (* WORK_MELDUNG *) ;



begin (* HAUPTPROGRAMM *)
  MELD_GESAMT := '' ;
  MELD_GESAMT_HEX := '' ;
  repeat
    READLN ( LOGFILE , ZEILE ) ;
    DATUM := SUBSTR ( ZEILE , 2 , 10 ) ;
    UHRZEIT := SUBSTR ( ZEILE , 13 , 8 ) ;
    MELDUNG := SUBSTR ( ZEILE , 34 , 80 ) ;
    if DATUM = ' ' then
      continue ;

  //**********************************************************
  // START PROGRAMM: Neue Meldung startet                     
  //**********************************************************

    if MELDUNG = 'START PROGRAMM' then
      begin

  //*******************************
  // ggf. alte Meldung abarbeiten  
  //*******************************

        WORK_MELDUNG ( XMLOUT , MELD_GESAMT , MELD_GESAMT_HEX ,
                       LAENGE_HEADER ) ;

  //***********************
  // Neue Meldung startet  
  //***********************

        ANZ_UML := 0 ;
        OFFS_MELDUNG := 0 ;
        MELD_GESAMT := '' ;
        MELD_GESAMT_HEX := '' ;
        MELD_DATUM := DATUM ;
        MELD_UHRZEIT := UHRZEIT ;
        WRITELN ( 'Start Meldung ' , MELD_DATUM , ' ' , MELD_UHRZEIT )
                  ;
        WRITELN ( XMLOUT ) ;
        WRITELN ( XMLOUT ) ;
        WRITELN ( XMLOUT ) ;
        WRITELN ( XMLOUT , 'Start Meldung ' , MELD_DATUM , ' ' ,
                  MELD_UHRZEIT ) ;
        continue ;
      end (* then *) ;

  //**********************************************************
  // Laenge der Meldung lt. Mainframe                         
  //**********************************************************

    if LEFT ( MELDUNG , 14 ) = 'ECHT-LAENGE = ' then
      begin
        ECHTLAENGE := SUBSTR ( MELDUNG , 15 ) ;
        ECHTLAENGE := TRIM ( ECHTLAENGE ) ;
        X := INDEX ( ECHTLAENGE , '.' ) ;
        while X > 0 do
          begin
            ECHTLAENGE := DELETE ( ECHTLAENGE , X , 1 ) ;
            X := INDEX ( ECHTLAENGE , '.' ) ;
          end (* while *) ;
        READSTR ( ECHTLAENGE , MELD_ECHTLAENGE ) ;
        WRITELN ( XMLOUT , 'Echtlaenge    = ' , MELD_ECHTLAENGE ) ;
        continue ;
      end (* then *) ;

  //**********************************************************
  // Laenge der Meldung alte Version                          
  //**********************************************************

    if LEFT ( MELDUNG , 13 ) = 'ECHTLAENGE = ' then
      begin
        ECHTLAENGE := SUBSTR ( MELDUNG , 14 ) ;
        ECHTLAENGE := TRIM ( ECHTLAENGE ) ;
        X := INDEX ( ECHTLAENGE , '.' ) ;
        while X > 0 do
          begin
            ECHTLAENGE := DELETE ( ECHTLAENGE , X , 1 ) ;
            X := INDEX ( ECHTLAENGE , '.' ) ;
          end (* while *) ;
        READSTR ( ECHTLAENGE , MELD_ECHTLAENGE ) ;
        WRITELN ( XMLOUT , 'Echtlaenge    = ' , MELD_ECHTLAENGE ) ;
        continue ;
      end (* then *) ;

  //**********************************************************
  // Nachrichten-Bestandteil                                  
  //**********************************************************

    if ( MELDUNG [ 2 ] = ' ' ) and ( MELDUNG [ 4 ] = ' ' ) then
      begin
        MELD1 := STR ( MELDUNG ) ;
        MELD_GESAMT := MELD_GESAMT || MELD1 ;
        READLN ( LOGFILE , ZEILE ) ;
        MELDUNG := SUBSTR ( ZEILE , 34 , 80 ) ;
        MELD2 := STR ( MELDUNG ) ;
        MELD_GESAMT_HEX := MELD_GESAMT_HEX || MELD2 ;
        for I := 0 to 39 do
          if SUBSTR ( MELD2 , I * 2 + 1 , 2 ) = '66' then
            begin
              if FALSE then
                begin
                  WRITELN ;
                  WRITELN ( 'UMLAUT:' ) ;
                  WRITELN ( MELD1 ) ;
                  WRITELN ( MELD2 ) ;
                  WRITELN ( OFFS_MELDUNG , I ) ;
                end (* then *) ;
              ANZ_UML := ANZ_UML + 1 ;
              POS_UMLAUTE [ ANZ_UML ] := OFFS_MELDUNG + I ;
            end (* then *) ;
        OFFS_MELDUNG := OFFS_MELDUNG + 40 ;
        continue ;
      end (* then *) ;
  until EOF ( LOGFILE ) ;

  //*****************************************
  // letzte verbleibende Meldung abarbeiten  
  //*****************************************

  WORK_MELDUNG ( XMLOUT , MELD_GESAMT , MELD_GESAMT_HEX , LAENGE_HEADER
                 ) ;
end (* HAUPTPROGRAMM *) .
