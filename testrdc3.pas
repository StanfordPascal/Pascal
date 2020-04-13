program TESTRDC3 ( SRCFIL , OUTPUT ) ;


label 99 ;


var SRCFIL : TEXT ;
    LINE : array [ 1 .. 250 ] of CHAR ;
    CC , LL : INTEGER ;
    CH : CHAR ;
    LC : INTEGER ;



procedure NEXTCH2 ;

(*****************************************)
(* read next character; process line end *)
(*****************************************)


   begin (* NEXTCH2 *)
     READ ( SRCFIL , CH ) ;
     WRITELN ( 'char = ' , CH , ' eoln = ' , EOLN ( SRCFIL ) ) ;
   end (* NEXTCH2 *) ;



procedure NEXTCH ;

(*****************************************)
(* read next character; process line end *)
(*****************************************)


   begin (* NEXTCH *)
     WRITELN ( 'at the beginning cc and ll: ' , CC , LL ) ;
     if CC = LL then
       begin
         if EOF ( SRCFIL )  (*[sam]*) then
           begin
             WRITELN ;
             WRITELN ( ' program incomplete' ) ;
             goto 99
           end (* then *) ;
         LC := LC + 1 ;
         WRITE ( LC : 5 , '  ' ) ;
         LL := 0 ;
         CC := 0 ;
         while not EOLN ( SRCFIL )  (*[sam]*) do
           begin
             LL := LL + 1 ;
             READ ( SRCFIL  (*[sam]*) , CH ) ;
             WRITE ( CH ) ;
             LINE [ LL ] := CH
           end (* while *) ;
         WRITELN ;
         LL := LL + 1 ;
         READ ( SRCFIL  (*[sam]*) , LINE [ LL ] ) ;
         WRITELN ( 'test line: <' , SUBSTR ( LINE , 1 , LL ) , '>' ) ;
       end (* then *) ;
     CC := CC + 1 ;
     CH := LINE [ CC ] ;
     WRITELN ( 'test char returned: <' , CH , '>' ) ;
   end (* NEXTCH *) ;



begin (* HAUPTPROGRAMM *)
  RESET ( SRCFIL ) ;
  CC := 0 ;
  LL := 0 ;
  LC := 0 ;
  repeat
    NEXTCH2
  until EOF ( SRCFIL ) ;
  99 :
  
end (* HAUPTPROGRAMM *) .
