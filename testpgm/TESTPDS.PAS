program TESTPDS ( INPUT , OUTPUT , PDS , PDSOUT ) ;


type CHAR8 = array [ 1 .. 8 ] of CHAR ;
     CHAR80 = array [ 1 .. 80 ] of CHAR ;


var PDS : TEXT ;
    PDSOUT : TEXT ;
    MEMBERNAME : CHAR8 ;
    ZEILE : CHAR80 ;



procedure TESTREAD ;

   begin (* TESTREAD *)
     RESET ( PDS ) ;
     WRITELN ( ' ' , 'nach reset' ) ;
     WRITELN ( ' ' , 'nach reset' ) ;
     WRITELN ( ' ' , 'nach reset' ) ;
     repeat
       WRITELN ( ' ' , 'vor readln' ) ;
       READLN ( PDS , ZEILE ) ;
       WRITELN ( ' ' , 'nach readln' ) ;
       WRITELN ( ' ' , ZEILE )
     until EOF ( PDS ) ;
   end (* TESTREAD *) ;



procedure TESTWRITE ;

   begin (* TESTWRITE *)
     RESET ( PDS ) ;
     REWRITE ( PDSOUT ) ;
     repeat
       READLN ( PDS , ZEILE ) ;
       WRITELN ( PDSOUT , ZEILE ) ;
     until EOF ( PDS ) ;
   end (* TESTWRITE *) ;



begin (* HAUPTPROGRAMM *)
  MEMBERNAME := 'KALENDER' ;
  WRITELN ( ' ' , 'vor assignmem' ) ;
  ASSIGNMEM ( PDS , ADDR ( MEMBERNAME ) , 8 ) ;
  WRITELN ( ' ' , 'nach assignmem' ) ;
  TESTREAD ;
  WRITELN ( ' ' , 'vor assignmem' ) ;
  ASSIGNMEM ( PDS , ADDR ( MEMBERNAME ) , 8 ) ;
  ASSIGNMEM ( PDSOUT , ADDR ( MEMBERNAME ) , 8 ) ;
  WRITELN ( ' ' , 'nach assignmem' ) ;
  TESTWRITE ;
end (* HAUPTPROGRAMM *) .
