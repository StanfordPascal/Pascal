program TESTSUB ( OUTPUT ) ;

//**********************************************************************
// Testprogramm fuer Strings bzw. VARCHARs                              
//                                                                      
// Oppolzer - 02.2018                                                   
//$A+                                                                   
//**********************************************************************



type TRANSLATE_TAB = array [ 1 .. 128 ] of CHAR ;


var C1 : CHAR ;
    C2 : CHAR ;
    C20 : CHAR ( 20 ) ;
    C20A : CHAR ( 20 ) ;
    VC20 : STRING ( 20 ) ;
    VC200 : STRING ( 200 ) ;
    VC1 : STRING ( 1 ) ;
    VC2000 : STRING ( 2000 ) ;
    LIMIT : INTEGER ;
    X : BOOLEAN ;
    P1 : STRING ( 20 ) ;
    P2 : STRING ( 200 ) ;
    I : INTEGER ;
    XP : -> CHAR ;
    TTAB : STRING ( 256 ) ;



function SUBSTR1 ( const SOURCE : STRING ; START : INTEGER ; LEN :
                 INTEGER ) : STRING ;

   type LENGTHF = 0 .. 32767 ;

   var X : INTEGER ;
       Z : STRING ( 100 ) ;
       P : ANYPTR ;
       Q : ANYPTR ;
       PLF : -> LENGTHF ;

   begin (* SUBSTR1 *)
     if LEN < 0 then
       begin
         if START > LENGTH ( SOURCE ) then
           EXIT ( 1201 ) ;
         LEN := LENGTH ( SOURCE ) - START + 1 ;
       end (* then *)
     else
       begin
         X := START + LEN - 1 ;
         if X > LENGTH ( SOURCE ) then
           EXIT ( 1201 ) ;
       end (* else *) ;
     if LEN > 100 then
       EXIT ( 1202 ) ;
     Z := '' ;
     P := ADDR ( Z ) ;
     P := PTRADD ( P , 2 ) ;
     PLF := P ;
     P := PTRADD ( P , 2 ) ;
     Q := ADDR ( SOURCE [ START ] ) ;
     PLF -> := LEN ;
     MEMCPY ( P , Q , LEN ) ;
     SUBSTR1 := Z ;
   end (* SUBSTR1 *) ;



function SUBSTR2 ( const SOURCE : STRING ; START : INTEGER ; LEN :
                 INTEGER ) : STRING ;

   type LENGTHF = 0 .. 32767 ;

   var X : INTEGER ;
       P : ANYPTR ;
       Q : ANYPTR ;

   begin (* SUBSTR2 *)
     if LEN < 0 then
       begin
         if START > LENGTH ( SOURCE ) then
           EXIT ( 1201 ) ;
         LEN := LENGTH ( SOURCE ) - START + 1 ;
       end (* then *)
     else
       begin
         X := START + LEN - 1 ;
         if X > LENGTH ( SOURCE ) then
           EXIT ( 1201 ) ;
       end (* else *) ;
     SUBSTR2 := REPEATSTR ( ' ' , LEN ) ;
     P := STRRESULTP ;
     Q := ADDR ( SOURCE [ START ] ) ;
     MEMCPY ( P , Q , LEN ) ;
   end (* SUBSTR2 *) ;



procedure TEST4 ;

   begin (* TEST4 *)
     P1 := 'Oppolzer' ;
     WRITE ( 'Zugriff auf String ueber Index .......: ' ) ;
     WRITELN ( P1 [ 1 ] ) ;
     WRITE ( 'Zugriff auf String ueber Index .......: ' ) ;
     WRITELN ( P1 [ 5 ] ) ;
     P2 := SUBSTR1 ( P1 , 3 , 4 ) ;
     WRITE ( 'p2 = Ergebnis von SUBSTR1 ............: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;
     P2 := SUBSTR1 ( 'Bernd ' || 'Oppolzer' , 5 , 7 ) ;
     WRITE ( 'p2 = Ergebnis von SUBSTR1 ............: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;
     P2 := SUBSTR1 ( 'Bernd ' || 'Oppolzer' , 5 , - 1 ) ;
     WRITE ( 'p2 = Ergebnis von SUBSTR1 ............: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;

     //*************************************************************
     // test SUBSTR2 - korrekte Variante mit STRRESULTP usw.        
     //*************************************************************

     P1 := 'Oppolzer' ;
     P2 := SUBSTR2 ( P1 , 3 , 4 ) ;
     WRITE ( 'p2 = Ergebnis von SUBSTR2 ............: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;
     P2 := SUBSTR2 ( 'Bernd ' || 'Oppolzer' , 5 , 7 ) ;
     WRITE ( 'p2 = Ergebnis von SUBSTR2 ............: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;
     P2 := SUBSTR2 ( 'Bernd ' || 'Oppolzer' , 5 , - 1 ) ;
     WRITE ( 'p2 = Ergebnis von SUBSTR2 ............: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;

     //*************************************************************
     // test SUBSTR                                                 
     //*************************************************************

     P1 := 'Oppolzer' ;
     P2 := SUBSTR ( P1 , 3 , 4 ) ;
     WRITE ( 'p2 = Ergebnis von SUBSTR .............: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;
     WRITE ( 'P1 nach SUBSTR (should not change) ...: ' ) ;
     WRITELN ( '<' , P1 , '>' ) ;
     P2 := SUBSTR ( 'Bernd ' || 'Oppolzer' , 5 , 7 ) ;
     WRITE ( 'p2 = Ergebnis von SUBSTR .............: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;
     P2 := SUBSTR ( 'Bernd ' || 'Oppolzer' , 5 ) ;
     WRITE ( 'p2 = Ergebnis von SUBSTR .............: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;
   end (* TEST4 *) ;



begin (* HAUPTPROGRAMM *)
  TEST4 ;
end (* HAUPTPROGRAMM *) .
