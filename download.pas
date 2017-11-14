program DOWNLOAD ( CONTROL , OUTPUT , PDS001 , PDS002 , PDS003 , PDS004
                   , PDS005 , PDS006 , PDS007 , PDS008 , PDS009 ) ;


type CHARPTR = -> CHAR ;


var CONTROL : TEXT ;
    PDS001 : TEXT ;
    PDS002 : TEXT ;
    PDS003 : TEXT ;
    PDS004 : TEXT ;
    PDS005 : TEXT ;
    PDS006 : TEXT ;
    PDS007 : TEXT ;
    PDS008 : TEXT ;
    PDS009 : TEXT ;
    ZEILE : array [ 1 .. 80 ] of CHAR ;
    X : INTEGER ;
    XFOUND : INTEGER ;
    FILENO : CHAR ;
    PMEM : CHARPTR ;
    PDSN : CHARPTR ;
    PENDDSN : CHARPTR ;
    LENDSN : INTEGER ;
    MEM : array [ 1 .. 8 ] of CHAR ;
    DSN : array [ 1 .. 44 ] of CHAR ;



procedure AUSGABE_FILE ( var F : TEXT ; PDSN : CHARPTR ; PMEMB :
                       CHARPTR ) ;

   var P : VOIDPTR ;
       CP : -> CHAR ;
       MEMBNAME : array [ 1 .. 8 ] of CHAR ;
       DSNAME : array [ 1 .. 44 ] of CHAR ;
       ZEILE : array [ 1 .. 80 ] of CHAR ;

   begin (* AUSGABE_FILE *)
     DSNAME := ' ' ;
     MEMCPY ( ADDR ( DSNAME ) , PDSN , SIZEOF ( DSNAME ) ) ;
     MEMCPY ( ADDR ( MEMBNAME ) , PMEMB , 8 ) ;
     ASSIGNMEM ( F , ADDR ( MEMBNAME ) , 8 ) ;
     RESET ( F ) ;
     P := FILEFCB ( F ) ;
     CP := PTRADD ( P , 32 ) ;
     if CP -> = '0' then
       return ;
     WRITELN ( 'FILE ' , DSNAME , 'MEMB ' , MEMBNAME ) ;
     while not EOF ( F ) do
       begin
         READLN ( F , ZEILE ) ;
         WRITELN ( 'DATA ' , ZEILE ) ;
       end (* while *) ;
     CLOSE ( F ) ;
   end (* AUSGABE_FILE *) ;



begin (* HAUPTPROGRAMM *)
  RESET ( CONTROL ) ;
  while not EOF ( CONTROL ) do
    begin
      READLN ( CONTROL , ZEILE ) ;
      XFOUND := 0 ;
      for X := 1 to 80 do
        if ZEILE [ X ] <> ' ' then
          begin
            XFOUND := X ;
            break
          end (* then *) ;
      if XFOUND = 0 then
        continue ;
      FILENO := ZEILE [ XFOUND ] ;
      X := XFOUND + 1 ;
      while ( X < 70 ) and ( ZEILE [ X ] = ' ' ) do
        X := X + 1 ;
      PDSN := ADDR ( ZEILE [ X ] ) ;
      while ( X < 70 ) and ( ZEILE [ X ] <> ' ' ) do
        X := X + 1 ;
      PENDDSN := ADDR ( ZEILE [ X ] ) ;
      LENDSN := PTRDIFF ( PENDDSN , PDSN ) ;
      while ( X < 70 ) and ( ZEILE [ X ] = ' ' ) do
        X := X + 1 ;
      PMEM := ADDR ( ZEILE [ X ] ) ;
      DSN := ' ' ;
      MEMCPY ( ADDR ( DSN ) , PDSN , LENDSN ) ;
      MEM := ' ' ;
      MEMCPY ( ADDR ( MEM ) , PMEM , 8 ) ;
      case FILENO of
        '1' : AUSGABE_FILE ( PDS001 , ADDR ( DSN ) , ADDR ( MEM ) ) ;
        '2' : AUSGABE_FILE ( PDS002 , ADDR ( DSN ) , ADDR ( MEM ) ) ;
        '3' : AUSGABE_FILE ( PDS003 , ADDR ( DSN ) , ADDR ( MEM ) ) ;
        '4' : AUSGABE_FILE ( PDS004 , ADDR ( DSN ) , ADDR ( MEM ) ) ;
        '5' : AUSGABE_FILE ( PDS005 , ADDR ( DSN ) , ADDR ( MEM ) ) ;
        '6' : AUSGABE_FILE ( PDS006 , ADDR ( DSN ) , ADDR ( MEM ) ) ;
        '7' : AUSGABE_FILE ( PDS007 , ADDR ( DSN ) , ADDR ( MEM ) ) ;
        '8' : AUSGABE_FILE ( PDS008 , ADDR ( DSN ) , ADDR ( MEM ) ) ;
        '9' : AUSGABE_FILE ( PDS009 , ADDR ( DSN ) , ADDR ( MEM ) ) ;
      end (* case *) ;
    end (* while *)
end (* HAUPTPROGRAMM *) .
