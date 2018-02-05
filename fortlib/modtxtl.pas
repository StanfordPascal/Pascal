program MODTXTL ( TXTIN , TXTLIB , TXTOUT ) ;


var TXTIN : TEXT ;
    TXTLIB : TEXT ;
    TXTOUT : TEXT ;
    LINE : CHAR ( 80 ) ;
    FLAG : CHAR ( 3 ) ;
    P : ANYPTR ;
    CP : -> CHAR ;


begin (* HAUPTPROGRAMM *)
  RESET ( TXTIN ) ;
  RESET ( TXTLIB ) ;
  REWRITE ( TXTOUT ) ;
  P := FILEFCB ( TXTIN ) ;
  CP := PTRADD ( P , 32 ) ;
  if CP -> <> '0' then
    begin
      while not EOF ( TXTIN ) do
        begin
          READLN ( TXTIN , LINE ) ;
          MEMCPY ( ADDR ( FLAG ) , ADDR ( LINE [ 2 ] ) , 3 ) ;
          if ( FLAG = 'ESD' ) or ( FLAG = 'RLD' ) or ( FLAG = 'TXT' )
          or ( FLAG = 'END' ) then
            WRITELN ( TXTOUT , LINE )
        end (* while *) ;
    end (* then *) ;
  while not EOF ( TXTLIB ) do
    begin
      READLN ( TXTLIB , LINE ) ;

  //*************************************
  // not yet possible on the mainframe:  
  // flag := substr (line, 2, 3);        
  //*************************************

      MEMCPY ( ADDR ( FLAG ) , ADDR ( LINE [ 2 ] ) , 3 ) ;
      if ( FLAG = 'ESD' ) or ( FLAG = 'RLD' ) or ( FLAG = 'TXT' ) or (
      FLAG = 'END' ) then
        WRITELN ( TXTOUT , LINE )
    end (* while *)
end (* HAUPTPROGRAMM *) .
