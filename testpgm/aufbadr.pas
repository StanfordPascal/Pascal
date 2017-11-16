program AUFBADR ( EING , AUSG ) ;

(********)
(*$A+   *)
(********)



type CHAR5 = array [ 1 .. 5 ] of CHAR ;
     CHAR30 = array [ 1 .. 30 ] of CHAR ;


var EING : TEXT ;
    AUSG : TEXT ;
    NAME : CHAR30 ;
    STRASSE : CHAR30 ;
    LANDKZ : CHAR ;
    PLZ : CHAR5 ;
    ORT : CHAR30 ;
    NAME2 : CHAR30 ;
    STRASSE2 : CHAR30 ;
    LANDKZ2 : CHAR ;
    PLZ2 : CHAR5 ;
    ORT2 : CHAR30 ;
    SPACE : INTEGER ;


begin (* HAUPTPROGRAMM *)
  RESET ( EING ) ;
  while not EOF ( EING ) do
    begin
      READ ( EING , NAME ) ;
      READ ( EING , STRASSE ) ;
      READ ( EING , LANDKZ ) ;
      READ ( EING , PLZ ) ;
      READ ( EING , ORT ) ;
      READLN ( EING ) ;
      if not EOF ( EING ) then
        begin
          READ ( EING , NAME2 ) ;
          READ ( EING , STRASSE2 ) ;
          READ ( EING , LANDKZ2 ) ;
          READ ( EING , PLZ2 ) ;
          READ ( EING , ORT2 ) ;
          READLN ( EING ) ;
        end (* then *)
      else
        begin
          NAME2 := ' ' ;
          STRASSE2 := ' ' ;
          LANDKZ2 := ' ' ;
          PLZ2 := ' ' ;
          ORT2 := ' ' ;
        end (* else *) ;
      WRITELN ( AUSG , NAME , ' ' : 10 , NAME2 ) ;
      WRITELN ( AUSG , STRASSE , ' ' : 10 , STRASSE2 ) ;
      SPACE := 4 ;
      if LANDKZ <> ' ' then
        begin
          WRITE ( AUSG , LANDKZ , '-' ) ;
          SPACE := SPACE - 2 ;
        end (* then *) ;
      WRITE ( AUSG , PLZ ) ;
      WRITE ( AUSG , ' ' ) ;
      WRITE ( AUSG , ORT ) ;
      WRITE ( AUSG , ' ' : SPACE ) ;
      if LANDKZ2 <> ' ' then
        begin
          WRITE ( AUSG , LANDKZ2 , '-' ) ;
        end (* then *) ;
      WRITE ( AUSG , PLZ2 ) ;
      WRITE ( AUSG , ' ' ) ;
      WRITE ( AUSG , ORT2 ) ;
      WRITELN ( AUSG ) ;
      WRITELN ( AUSG ) ;
      WRITELN ( AUSG ) ;
      WRITELN ( AUSG ) ;
    end (* while *)
end (* HAUPTPROGRAMM *) .
