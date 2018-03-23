program TESTSTR1 ( OUTPUT ) ;

//**********************************************************************
//$A+                                                                   
//**********************************************************************



var S : STRING ( 20 ) ;
    C : CHAR ( 20 ) ;
    I : INTEGER ;
    C1 : CHAR ;
    C2 : CHAR ;
    C20 : CHAR ( 20 ) ;
    C20A : CHAR ( 20 ) ;
    VC20 : STRING ( 20 ) ;
    VC200 : STRING ( 200 ) ;
    X : BOOLEAN ;
    P1 : STRING ( 20 ) ;
    P2 : STRING ( 200 ) ;



function MEMCMPX ( X : ANYPTR ; Y : ANYPTR ; L : INTEGER ) : INTEGER ;

   var PLINKS : -> CHAR ;
       PRECHTS : -> CHAR ;
       PLIMIT : -> CHAR ;
       RESULT : INTEGER ;

   begin (* MEMCMPX *)
     PLINKS := X ;
     PRECHTS := Y ;
     PLIMIT := PTRADD ( PLINKS , L ) ;
     RESULT := 0 ;
     while PTRDIFF ( PLIMIT , PLINKS ) > 0 do
       begin
         if PLINKS -> < PRECHTS -> then
           begin
             RESULT := - 1 ;
             break
           end (* then *)
         else
           if PLINKS -> > PRECHTS -> then
             begin
               RESULT := 1 ;
               break
             end (* then *)
           else
             begin
               PLINKS := PTRADD ( PLINKS , 1 ) ;
               PRECHTS := PTRADD ( PRECHTS , 1 ) ;
             end (* else *)
       end (* while *) ;
     MEMCMPX := RESULT
   end (* MEMCMPX *) ;



procedure TEIL1 ;

   begin (* TEIL1 *)
     C := 'Bernd' ;
     WRITELN ( 'c         = ' , C ) ;
     S := 'Oppolzer' ;
     WRITELN ( 'maxlength = ' , MAXLENGTH ( S ) ) ;
     WRITELN ( 'length    = ' , LENGTH ( S ) ) ;
     C := S ;
     WRITELN ( 'c         = ' , C ) ;
     for I := 1 to LENGTH ( S ) do
       WRITE ( S [ I ] , ' ' ) ;
     WRITELN ;

     //*****************************************************************
     // tests aus testvarc                                              
     //*****************************************************************

     C1 := 'A' ;
     C2 := C1 ;
     C20 := 'Test' ;
     VC20 := STR ( 'A' ) ;
     WRITE ( 'VC20 nach Zuweisung STR (''A'') ........: ' ) ;
     WRITELN ( '<' , VC20 , '>' ) ;
     VC20 := STR ( C1 ) ;
     WRITE ( 'VC20 nach Zuweisung STR (C1) .........: ' ) ;
     WRITELN ( '<' , VC20 , '>' ) ;
     VC20 := 'A' ;
     WRITE ( 'VC20 nach Zuweisung ''A'' ..............: ' ) ;
     WRITELN ( '<' , VC20 , '>' ) ;
     VC20 := 'Test Varchar' ;
     WRITE ( 'VC20 nach Zuweisung const char array .: ' ) ;
     WRITELN ( '<' , VC20 , '>' ) ;

     //*****************************************************************
     // ist nicht zulaessig, muss Konstante sein oder Funktion STR      
     //*****************************************************************
     // VC20 := C20 ;  /* sollte Fehler sein */                         
     //*****************************************************************
     // so korrekt                                                      
     //*****************************************************************

     VC20 := STR ( C20 ) ;
     WRITE ( 'VC20 nach Zuweisung STR (C20) ........: ' ) ;
     WRITELN ( '<' , VC20 , '>' ) ;

     //*****************************************************************
     // muss erlaubt sein, mit Blanks auffuellen wie gehabt ...         
     //*****************************************************************

     VC20 := 'Bernd' ;
     WRITE ( 'VC20 nach Zuweisung Bernd ............: ' ) ;
     WRITELN ( '<' , VC20 , '>' ) ;
     C20 := VC20 ;
     WRITELN ( 'C20 nach Zuweisung VC20 ..............: ' , C20 ) ;
     C20 := 'Bernd ' || 'Oppolzer' ;
     WRITELN ( 'C20 nach Zuweisung String Expression .: ' , C20 ) ;

     //*****************************************************************
     // Verkettung usw.                                                 
     //*****************************************************************

     VC200 := VC20 || VC20 ;
     WRITE ( 'VC200 nach Verkettung ................: ' ) ;
     WRITELN ( '<' , VC200 , '>' ) ;
     VC200 := VC20 || ' Oppolzer' ;
     WRITE ( 'VC200 nach Verkettung ................: ' ) ;
     WRITELN ( '<' , VC200 , '>' ) ;
     VC200 := VC20 || ' Oppolzer' || ' Leinfelden' ;
     WRITE ( 'VC200 nach Verkettung ................: ' ) ;
     WRITELN ( '<' , VC200 , '>' ) ;
     VC200 := VC20 || ( ' Oppolzer' || ' Leinfelden' ) ;
     WRITE ( 'VC200 nach Verkettung (Klammer hinten): ' ) ;
     WRITELN ( '<' , VC200 , '>' ) ;
     VC200 := ( VC20 || ' Oppolzer' ) || ' Leinfelden' ;
     WRITE ( 'VC200 nach Verkettung ................: ' ) ;
     WRITELN ( '<' , VC200 , '>' ) ;
     VC200 := ( ( VC20 || ' Oppolzer' ) || ' Leinfelden' ) ;
     WRITE ( 'VC200 nach Verkettung ................: ' ) ;
     WRITELN ( '<' , VC200 , '>' ) ;
     VC200 := VC20 || STR ( ' Oppolzer' ) || STR ( ' Leinfelden' ) ;
     WRITE ( 'VC200 nach Verkettung (mit STR Funk.).: ' ) ;
     WRITELN ( '<' , VC200 , '>' ) ;
     VC200 := VC20 || ' ' || VC20 ;
     WRITE ( 'VC200 nach Verkettung (mit Blank) ....: ' ) ;
     WRITELN ( '<' , VC200 , '>' ) ;
     VC200 := VC20 || ' dazwischen ' || VC20 ;
     WRITE ( 'VC200 nach Verkettung ................: ' ) ;
     WRITELN ( '<' , VC200 , '>' ) ;
     VC200 := VC20 || ' dazwischen ' || STR ( C20 ) ;
     WRITE ( 'VC200 nach Verkettung ................: ' ) ;
     WRITELN ( '<' , VC200 , '>' ) ;
     VC200 := STR ( C20 ) || ' dazwischen ' || VC20 ;
     WRITE ( 'VC200 nach Verkettung ................: ' ) ;
     WRITELN ( '<' , VC200 , '>' ) ;

     //*****************************************************************
     // Zuweisungen einfach oder Expression                             
     //*****************************************************************

     VC20 := 'Test-String' ;
     WRITE ( 'VC20 nach Zuweisung Konstante ........: ' ) ;
     WRITELN ( '<' , VC20 , '>' ) ;
     VC200 := VC20 ;
     WRITE ( 'VC200 nach Zuweisung VC20 ............: ' ) ;
     WRITELN ( '<' , VC200 , '>' ) ;
     VC200 := VC20 || ' mit Verkettung' ;
     WRITE ( 'VC200 nach Zuweisung Expression ......: ' ) ;
     WRITELN ( '<' , VC200 , '>' ) ;
     VC20 := 'Bernd' ;
   end (* TEIL1 *) ;



procedure TEIL2 ;

   var ZZ : STRING ( 20 ) ;

   begin (* TEIL2 *)

     //************************************************************
     // Vergleiche von Strings                                     
     //************************************************************

     X := C20 = 'Bernd Oppolzer' ;
     WRITELN ( 'c20 = ''Bernd Oppolzer'' ...............: ' , X ) ;
     X := C20 = 'Bernd' ;
     WRITELN ( 'c20 = ''Bernd'' ........................: ' , X ) ;
     X := C20 = 'Bernd      ' ;
     WRITELN ( 'c20 = ''Bernd      '' ..................: ' , X ) ;
     ZZ := 'Bernd' ;
     WRITELN ( 'vc20 = <' , VC20 , '>' ) ;
     WRITELN ( 'zz   = <' , ZZ , '>' ) ;
     X := VC20 = ZZ ;
     WRITELN ( 'vc20 = zz ............................: ' , X ) ;
     ZZ := 'Bornd' ;
     WRITELN ( 'vc20 = <' , VC20 , '>' ) ;
     WRITELN ( 'zz   = <' , ZZ , '>' ) ;
     X := VC20 = ZZ ;
     WRITELN ( 'vc20 = zz ............................: ' , X ) ;
     X := VC20 = 'Bernd' ;
     WRITELN ( 'vc20 = ''Bernd'' .......................: ' , X ) ;
     X := VC20 = 'Berndx' ;
     WRITELN ( 'vc20 = ''Berndx'' ......................: ' , X ) ;
     X := VC20 = 'Bornd' ;
     WRITELN ( 'vc20 = ''Bornd'' .......................: ' , X ) ;
     X := ( VC20 = 'Bernd' ) ;
     WRITELN ( 'vc20 = ''Bernd'' .......................: ' , X ) ;
     X := VC20 || ' ' = 'Bernd' || ' ' ;
     WRITELN ( 'vc20 = ''Bernd'' .......................: ' , X ) ;
     X := VC20 = 'Bernd ' ;
     WRITELN ( 'vc20 = ''Bernd '' ......................: ' , X ) ;
     X := C20 = 'Bernd' ;
     WRITELN ( 'c20 = ''Bernd'' ........................: ' , X ) ;
     X := C20 = 'Bernd      ' ;
     WRITELN ( 'c20 = ''Bernd      '' ..................: ' , X ) ;
     C20A := 'Oppolzer' ;
     WRITELN ( 'c20 = ................................: ' , C20 ) ;
     WRITELN ( 'c20A = ...............................: ' , C20A ) ;
     WRITELN ( 'VC20 = ...............................: ' , VC20 ) ;
     X := C20A > C20 ;
     WRITELN ( 'c20a > c20 ...........................: ' , X ) ;
     X := C20A < C20 ;
     WRITELN ( 'c20a < c20 ...........................: ' , X ) ;
     X := STR ( C20 ) > VC20 ;
     WRITELN ( 'c20 > vc20 ...........................: ' , X ) ;
     X := STR ( C20 ) < VC20 ;
     WRITELN ( 'c20 < vc20 ...........................: ' , X ) ;
     X := VC20 < C20 ;
     WRITELN ( 'c20 > vc20 ??? .......................: ' , X ) ;
     X := VC20 > C20 ;
     WRITELN ( 'c20 < vc20 ??? .......................: ' , X ) ;
     X := VC20 = C20 ;
     WRITELN ( 'c20 = vc20 ...........................: ' , X ) ;
     X := STR ( C20A ) > VC20 ;
     WRITELN ( 'c20a > vc20 ..........................: ' , X ) ;
     X := STR ( C20A ) < VC20 ;
     WRITELN ( 'c20a < vc20 ..........................: ' , X ) ;
     X := VC20 < C20A ;
     WRITELN ( 'c20a > vc20 ..........................: ' , X ) ;
     X := VC20 > C20A ;
     WRITELN ( 'c20a < vc20 ..........................: ' , X ) ;
     if VC20 || ' ' = 'Bernd' || ' ' then
       WRITELN ( 'success !!' ) ;
   end (* TEIL2 *) ;



procedure STRPROC ( X : STRING ( 20 ) ; var Y : STRING ; A : STRING (
                  200 ) ; var B : STRING ) ;

   begin (* STRPROC *)
     if TRUE then
       begin
         WRITELN ( 'maxlength (x) = ' , MAXLENGTH ( X ) ) ;
         WRITELN ( 'maxlength (y) = ' , MAXLENGTH ( Y ) ) ;
         WRITELN ( 'maxlength (a) = ' , MAXLENGTH ( A ) ) ;
         WRITELN ( 'maxlength (b) = ' , MAXLENGTH ( B ) ) ;
       end (* then *) ;
     B := X || ' Oppolzer' ;
     Y := A ;
     WRITELN ( 'In STRPROC - X = <' , X , '>' ) ;
     WRITELN ( 'In STRPROC - Y = <' , Y , '>' ) ;
     WRITELN ( 'In STRPROC - A = <' , A , '>' ) ;
     WRITELN ( 'In STRPROC - B = <' , B , '>' ) ;
   end (* STRPROC *) ;



function STRFUNC ( X : STRING ( 20 ) ; Y : STRING ( 20 ) ) : STRING ;

   begin (* STRFUNC *)
     STRFUNC := X || '/' || Y ;
   end (* STRFUNC *) ;



function IFUNC ( X : STRING ( 20 ) ; Y : STRING ( 20 ) ) : INTEGER ;

   begin (* IFUNC *)
     IFUNC := LENGTH ( X || '/' || Y ) ;
   end (* IFUNC *) ;



function STRFUNC2 ( const X : STRING ; const Y : STRING ) : STRING ;

   begin (* STRFUNC2 *)
     STRFUNC2 := X || '/' || Y ;
   end (* STRFUNC2 *) ;



function STRFUNC3 ( var X : STRING ; var Y : STRING ) : STRING ;

   var ERG : STRING ( 200 ) ;

   begin (* STRFUNC3 *)
     WRITELN ( 'strfunc3: x = <' , X , '>' ) ;
     WRITELN ( 'strfunc3: y = <' , Y , '>' ) ;
     ERG := X || '/' || Y ;
     WRITELN ( 'strfunc3: erg = <' , ERG , '>' ) ;
     STRFUNC3 := ERG ;
   end (* STRFUNC3 *) ;



function STRFUNC5 ( var X : STRING ; var Y : STRING ) : STRING ;

   begin (* STRFUNC5 *)
     STRFUNC5 := STRFUNC3 ( X , Y ) ;
   end (* STRFUNC5 *) ;



begin (* HAUPTPROGRAMM *)
  TEIL1 ;
  TEIL2 ;

  //******************************************************************
  // verkettung direkt in Writeln ...                                 
  //******************************************************************

  VC200 := VC20 || VC20 ;
  WRITE ( 'VC200 nach Verkettung ................: ' ) ;
  WRITELN ( '<' , VC200 , '>' ) ;
  WRITE ( 'Verkettung direkt ausgeben ...........: ' ) ;
  WRITELN ( '<' , VC20 || VC20 , '>' ) ;
  WRITE ( 'Verkettung nochmal direkt ausgeben ...: ' ) ;
  WRITELN ( '<' , VC20 || ' ' || C20A , '>' ) ;

  //******************************************************************
  // tests mit prozeduren, call by value und by reference             
  //******************************************************************

  WRITELN ( 'Aufruf STRPROC' ) ;
  STRPROC ( 'Bernd' , P1 , 'xxxxxxxxxxxxxxx' , P2 ) ;
  WRITELN ( 'zurÅck von STRPROC' ) ;
  WRITE ( 'P1 nach STRPROC ......................: ' ) ;
  WRITELN ( '<' , P1 , '>' ) ;
  WRITE ( 'P2 nach STRPROC ......................: ' ) ;
  WRITELN ( '<' , P2 , '>' ) ;

  //******************************************************************
  // noch ein Test, unpassende var-Parameter                          
  //******************************************************************

  WRITELN ( 'Aufruf STRPROC' ) ;
  STRPROC ( 'Bernd' , P2 , 'xxxxxxxxxxxxxxx' , P1 ) ;
  WRITELN ( 'zurÅck von STRPROC' ) ;
  WRITE ( 'P1 nach STRPROC ......................: ' ) ;
  WRITELN ( '<' , P1 , '>' ) ;
  WRITE ( 'P2 nach STRPROC ......................: ' ) ;
  WRITELN ( '<' , P2 , '>' ) ;

  //******************************************************************
  // length und maxlength ausprobieren                                
  //******************************************************************

  WRITE ( 'maxlength von einfachem char .........: ' ) ;
  WRITELN ( MAXLENGTH ( 'a' ) ) ;
  WRITE ( 'maxlength von char-Konstante .........: ' ) ;
  WRITELN ( MAXLENGTH ( 'Bernd' ) ) ;
  WRITE ( 'maxlength von char-Variable ..........: ' ) ;
  WRITELN ( MAXLENGTH ( C1 ) ) ;
  WRITE ( 'maxlength von char-array .............: ' ) ;
  WRITELN ( MAXLENGTH ( C20A ) ) ;
  WRITE ( 'maxlength von string .................: ' ) ;
  WRITELN ( MAXLENGTH ( VC20 ) ) ;
  WRITE ( 'maxlength von string-Expression ......: ' ) ;
  WRITELN ( MAXLENGTH ( VC20 || ' ' || VC20 ) ) ;
  WRITE ( 'length von einfachem char ............: ' ) ;
  WRITELN ( LENGTH ( 'a' ) ) ;
  WRITE ( 'length von char-konstante ............: ' ) ;
  WRITELN ( LENGTH ( 'bernd' ) ) ;
  WRITE ( 'length von char-variable .............: ' ) ;
  WRITELN ( LENGTH ( C1 ) ) ;
  WRITE ( 'length von char-array ................: ' ) ;
  WRITELN ( LENGTH ( C20A ) ) ;
  WRITE ( 'length von string ....................: ' ) ;
  WRITELN ( LENGTH ( VC20 ) ) ;
  WRITE ( 'length von string-expression .........: ' ) ;
  WRITELN ( LENGTH ( VC20 || ' ' || VC20 ) ) ;

  //******************************************************************
  // Funktion mit string ergebnis ausprobieren                        
  //******************************************************************

  P1 := 'Oppolzer' ;
  I := IFUNC ( VC20 , P1 ) ;
  WRITE ( 'funktion mit Integer Ergebnis ........: ' ) ;
  WRITELN ( I ) ;
  P1 := 'Oppolzer' ;
  P2 := STRFUNC ( VC20 , P1 ) ;
  WRITE ( 'p2 = funktion mit String Ergebnis ....: ' ) ;
  WRITELN ( '<' , P2 , '>' ) ;
  P2 := 'Test ' || STRFUNC ( VC20 , P1 ) || ' nochmal Test' ;
  WRITE ( 'p2 = kombiniert mit Funktion .........: ' ) ;
  WRITELN ( '<' , P2 , '>' ) ;
  WRITE ( 'String Ergebnis direkt ...............: ' ) ;
  WRITELN ( '<' , STRFUNC ( VC20 , P1 ) , '>' ) ;
  WRITE ( 'String Ergebnis mit Konstanten .......: ' ) ;
  WRITELN ( '<' , STRFUNC ( 'Bernd ' , ' Oppolzer' ) , '>' ) ;

  //******************************************************************
  // test Funktion mit const String parametern                        
  //******************************************************************

  P1 := 'Oppolzer' ;
  P2 := STRFUNC3 ( VC20 , P1 ) ;
  WRITE ( 'p2 = funktion mit var String .........: ' ) ;
  WRITELN ( '<' , P2 , '>' ) ;
  P1 := 'Oppolzer' ;
  P2 := STRFUNC5 ( VC20 , P1 ) ;
  WRITE ( 'p2 = funktion mit var String (2 mal) .: ' ) ;
  WRITELN ( '<' , P2 , '>' ) ;
  P1 := 'Oppolzer' ;
  P2 := STRFUNC2 ( VC20 , P1 ) ;
  WRITE ( 'p2 = funktion mit const String .......: ' ) ;
  WRITELN ( '<' , P2 , '>' ) ;
  if FALSE then
    begin
      P1 := 'Oppolzer' ;
      P2 := STRFUNC2 ( VC20 || ' ' , ' Oppolzer' ) ;
      WRITE ( 'p2 = funktion mit const String .......: ' ) ;
      WRITELN ( '<' , P2 , '>' ) ;
    end (* then *)
end (* HAUPTPROGRAMM *) .
