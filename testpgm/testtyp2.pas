program TESTCON ;

//****************************************************************
//$A+                                                             
//****************************************************************
// - Feste CHARs an CONST Strings uebergeben geht schief          
// - muss mit STR gemacht werden                                  
// - auch bei TRIM und anderen BUILTIN Funktionen                 
// - unabhaengig davon funktioniert die Rueckgabe trotzdem nicht  
//****************************************************************



type REC = record
             A : INTEGER ;
             B : INTEGER ;
             C : CHAR ( 20 ) ;
           end ;
     PC20 = -> CHAR ( 20 ) ;
     SHORTINT = - 32000 .. 32000 ;


var T1 : REC :=
         ( 12 , 15 , 'Bernd' ) ;
    PR : -> REC ;
    P2 : -> REC ;
    PX : -> CHAR ( 20 ) ;
    CX : CHAR ( 20 ) ;
    S : STRING ( 20 ) ;
    S2 : STRING ( 20 ) ;



function XTRIM ( const S1 : STRING ) : STRING ;

   var LEN : INTEGER ;
       LS1 : INTEGER ;
       CP : -> CHAR ;
       SP1 : -> SHORTINT ;
       SP2 : -> SHORTINT ;

   begin (* XTRIM *)
     LS1 := LENGTH ( S1 ) ;
     WRITELN ( 'xtrim: ls1 = ' , LS1 : 1 ) ;
     if LS1 = 0 then
       begin
         XTRIM := S1 ;
         return ;
       end (* then *) ;
     CP := ADDR ( S1 [ LS1 ] ) ;
     LEN := LS1 ;
     while LEN > 0 do
       begin
         if CP -> <> ' ' then
           break ;
         CP := PTRADD ( CP , - 1 ) ;
         LEN := LEN - 1 ;
       end (* while *) ;
     CP := ADDR ( S1 [ 1 ] ) ;
     while LEN > 0 do
       begin
         if CP -> <> ' ' then
           break ;
         CP := PTRADD ( CP , 1 ) ;
         LEN := LEN - 1 ;
       end (* while *) ;
     WRITELN ( 'xtrim: len = ' , LEN : 1 ) ;
     XTRIM := REPEATSTR ( ' ' , LEN ) ;
     MEMCPY ( STRRESULTP , CP , LEN ) ;
     WRITELN ( 'xtrim: strres = ' , STRRESULTP ) ;
     CP := STRRESULTP ;
     WRITELN ( 'xtrim: strres = ' , CP -> ) ;
     WRITELN ( 'xtrim: res = ' , RESULTP ) ;
     SP1 := PTRADD ( RESULTP , 0 ) ;
     SP2 := PTRADD ( RESULTP , 2 ) ;
     WRITELN ( 'xtrim: res1 = ' , SP1 -> ) ;
     WRITELN ( 'xtrim: res2 = ' , SP2 -> ) ;
   end (* XTRIM *) ;



procedure TCON ( X : record A1 : INTEGER ; B1 : INTEGER ; C1 : CHAR (
               20 ) ; end ; const Y : record A1 : INTEGER ; B1 :
               INTEGER ; C1 : CHAR ( 20 ) ; end ; var Z : record A1 :
               INTEGER ; B1 : INTEGER ; C1 : CHAR ( 20 ) ; end ) ;

   begin (* TCON *)
     WRITELN ( X . A1 , X . B1 , X . C1 ) ;
     WRITELN ( Y . A1 , Y . B1 , Y . C1 ) ;
     WRITELN ( Z . A1 , Z . B1 , Z . C1 ) ;
   end (* TCON *) ;



function TPTR ( var Y : -> REC ) : -> REC ;

   begin (* TPTR *)
     TPTR := Y
   end (* TPTR *) ;



function T2 ( X : -> CHAR ( 20 ) ) : -> CHAR ( 20 ) ;

   begin (* T2 *)
     T2 := X
   end (* T2 *) ;



function T3 ( X : PC20 ) : PC20 ;

   begin (* T3 *)
     T3 := X
   end (* T3 *) ;



begin (* HAUPTPROGRAMM *)
  TCON ( T1 , T1 , T1 ) ;
  if TRUE then
    begin
      P2 := ADDR ( T1 ) ;
      PR := TPTR ( P2 ) ;
      WRITELN ( P2 ) ;
      WRITELN ( PR ) ;
    end (* then *) ;
  WRITELN ( T2 ( ADDR ( T1 . C ) ) ) ;
  CX := T2 ( ADDR ( T1 . C ) ) -> ;
  WRITELN ( 'CX = ' , CX ) ;
  CX := T3 ( ADDR ( T1 . C ) ) -> ;
  WRITELN ( 'CX = ' , CX ) ;
  S := STR ( CX ) ;
  S2 := XTRIM ( S ) ;
  WRITELN ( 'maxlength (s2) = ' , MAXLENGTH ( S2 ) ) ;
  WRITELN ( 'length (s2) = ' , LENGTH ( S2 ) ) ;
  WRITELN ( '<' , S2 , '>' ) ;
  if FALSE then
    begin
      PX := T2 ( ADDR ( T1 . C ) ) ;
      WRITELN ( PX -> ) ;
      S := XTRIM ( STR ( PX -> ) ) ;
      WRITELN ( 'maxlength (s) = ' , MAXLENGTH ( S ) ) ;
      WRITELN ( 'length (s) = ' , LENGTH ( S ) ) ;
      WRITELN ( '<' , S , '>' ) ;
      WRITELN ( 'SUBSTR mit 0: ' , SUBSTR ( S , 1 , 0 ) ) ;
      WRITELN ( 'hat funktioniert' ) ;
      WRITELN ( 'RIGHT mit 0: ' , RIGHT ( S , 0 ) ) ;
      WRITELN ( 'hat funktioniert' ) ;
      WRITELN ( 'LEFT mit 0: ' , LEFT ( S , 0 ) ) ;
      WRITELN ( 'hat funktioniert' ) ;
    end (* then *)
end (* HAUPTPROGRAMM *) .
