program TESTRDD ( INPFILE , OUTPUT ) ;


type BYTE = record
              CONT : CHAR
            end ;


var INPFILE : FILE of BYTE ;
    B : BYTE ;
    CH : CHAR ;


begin (* HAUPTPROGRAMM *)
  RESET ( INPFILE ) ;
  while not EOF ( INPFILE ) do
    begin
      READ ( INPFILE , B ) ;
      CH := B . CONT ;
      WRITE ( 'gelesen: ' ) ;
      if ORD ( CH ) > ORD ( ' ' ) then
        WRITE ( CH )
      else
        WRITE ( ' ' ) ;
      WRITE ( ORD ( CH ) : 4 ) ;
      CH := INPFILE -> . CONT ;
      WRITELN ( ORD ( CH ) : 4 ) ;
    end (* while *) ;
end (* HAUPTPROGRAMM *) .
