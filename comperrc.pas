program CHECKPLZ ( PLZFILE , OUTPUT ) ;


var PLZFILE : TEXT ;
    ZEILE : STRING ( 100 ) ;
    ANZPART : INTEGER ;
    PART : array [ 1 .. 10 ] of STRING ( 40 ) ;
    IX : INTEGER ;
    X : INTEGER ;
    MAXW : INTEGER := 0 ;
    FOUND : BOOLEAN ;
    ort : string ( 40 );
    ort_norm : string ( 40 );
    ort_ersatz : string ( 40 );
    ort_major  : string ( 40 );

    //*****************************************************
    // tabellen:
    // Tabelle der Bundeslaender und ihrer Abkuerzungen
    // Tabelle der Umlaute in UTF-8 und normal und Ersatz
    //*****************************************************



const LANDTAB : array [ 1 .. 16 ] of STRING ( 40 ) =       //
      ( 'Baden-W' X'c3bc' 'rttemberg' ,                    //
        'Bayern' ,                                         //
        'Berlin' ,                                         //
        'Brandenburg' ,                                    //
        'Bremen' ,                                         //
        'Hamburg' ,                                        //
        'Hessen' ,                                         //
        'Mecklenburg-Vorpommern' ,                         //
        'Niedersachsen' ,                                  //
        'Nordrhein-Westfalen' ,                            //
        'Rheinland-Pfalz' ,                                //
        'Saarland' ,                                       //
        'Sachsen' ,                                        //
        'Sachsen-Anhalt' ,                                 //
        'Schleswig-Holstein' ,                             //
        'Th' X'c3bc' 'ringen' ) ;                          //
      LANDKURZ : array [ 1 .. 16 ] of STRING ( 10 ) =      //
      ( 'BWL' ,                                            //
        'BYL' ,                                            //
        'B  ' ,                                            //
        'BRA' ,                                            //
        'BR ' ,                                            //
        'HH ' ,                                            //
        'HES' ,                                            //
        'MVP' ,                                            //
        'NIE' ,                                            //
        'NRW' ,                                            //
        'RP ' ,                                            //
        'SAR' ,                                            //
        'SAC' ,                                            //
        'SAN' ,                                            //
        'SLH' ,                                            //
        'TH ' ) ;                                          //
      UML_UTF : array [ 1 .. 7 ] of STRING ( 2 ) =
      ( '' , '' , X'c3bc' , '' , '' , '' , '' ) ;
      UML_NORM : array [ 1 .. 7 ] of CHAR =
      ( 'ä' , 'ö' , 'ü' , 'Ä' , 'Ö' , 'Ü' , 'ß' ) ;
      UML_ERS : array [ 1 .. 7 ] of STRING ( 2 ) =
      ( 'ae' , 'oe' , 'ue' , 'Ae' , 'Oe' , 'Ue' , 'ss' ) ;


procedure modify (var x : string ;
                  const from: string ;
                  to : char);

var y : string [40];

begin
  pos := index ( x, from);
  while pos <> 0 do
  begin
     x [pos] := to ;
     y := left (x, pos) || substr (x, pos + 2);
     x := y;
     pos := index ( x, from);
  end ;

end ;


procedure modify_inplace (var x : string ;
                  const from: string ;
                  const to : string );

begin
  pos := index ( x, from);
  while pos <> 0 do
  begin
     x [pos] := to [1];
     x [pos + 1] := to [2];
     pos := index ( x, from);
  end ;
end ;


procedure modify_ort (const ort: string;
                      var ort_norm : string ;
                      var ort_ers : string );

var l1: string [40];
    l2: string [40];
    i: integer ;


begin
  l1 := ort ;
  l2 := ort ;
  for i:= 1 to 7 do
  if uml_utf [i] <> '' then
  begin
     modify (l1, uml_utf [i], uml_norm [i]);
     modify_inplace (l2, uml_utf [i], uml_ers [i]);
  end ;
  ort_norm := l1;
  ort_ers := l2;
end ;


begin (* HAUPTPROGRAMM *)
  repeat

  //**************************************************
  // read line from file from suche-postleitzahl.org
  //**************************************************

    READLN ( PLZFILE , ZEILE ) ;
    IX := 0 ;
    while TRUE do
      begin
        X := INDEX ( ZEILE , ',' ) ;
        if X = 0 then
          begin
            IX := IX + 1 ;
            PART [ IX ] := TRIM ( ZEILE ) ;
            break ;
          end (* then *) ;
        IX := IX + 1 ;
        PART [ IX ] := LEFT ( ZEILE , X - 1 ) ;
        ZEILE := SUBSTR ( ZEILE , X + 1 ) ;
      end (* while *) ;

  //*********************
  // ignore header line
  //*********************

    if PART [ 1 ] = 'osm_id' then
      continue ;
    ANZPART := IX ;

  //************************************
  // error if number of parts is not 4
  //************************************

    if ANZPART <> 4 then
      begin
        WRITELN ( '+++ Fehler: ANZPART ungleich 4' ) ;
        EXIT ( 8 ) ;
      end (* then *) ;

  //**************************************
  // translate bundesland to shorter rep
  //**************************************

    FOUND := FALSE ;
    for IX := 1 to 16 do
      if PART [ 4 ] = LANDTAB [ IX ] then
        begin
          PART [ 4 ] := LANDKURZ [ IX ] ;
          FOUND := TRUE ;
        end (* then *) ;

  //************************************
  // error if bundesland not found
  //************************************

    if ANZPART <> 4 then
      begin
        WRITELN ( '+++ Fehler: Bundesland nicht gefunden' ) ;
        EXIT ( 8 ) ;
      end (* then *) ;

  //*************************************
  // check out maximum length of ort
  //*************************************

    if MAXW < LENGTH ( PART [ 2 ] ) then
      MAXW := LENGTH ( PART [ 2 ] ) ;
    ort := part [2];
    modify_ort (ort, ort_norm, ort_ersatz);
    major_ort (ort_ersatz, ort_major);


  //*************************************
  // write out content with fixed delim
  //*************************************

    WRITELN ( PART [ 3 ] : - 5 , ' ' , PART [ 4 ] : - 3 , ' ' , PART [
              2 ] : - 40 , ' ' , PART [ 1 ] : 10 ) ;
  until EOF ( PLZFILE ) ;
end (* HAUPTPROGRAMM *) .
