program TESTPDS ( INPUT , OUTPUT , PDS ) ;


type CHAR8 = array [ 1 .. 8 ] of CHAR ;
     CHAR80 = array [ 1 .. 80 ] of CHAR ;


var PDS : TEXT ;
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



begin (* HAUPTPROGRAMM *)
  MEMBERNAME := 'RUNKAL' ;
  WRITELN ( ' ' , 'vor assignmem' ) ;
  ASSIGNMEM ( PDS , ADDR ( MEMBERNAME ) , 8 ) ;
  WRITELN ( ' ' , 'nach assignmem' ) ;
  TESTREAD
end (* HAUPTPROGRAMM *) .
