program TESTVARC ( OUTPUT ) ;

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
    VC2000 : STRING ( 2000 ) ;
    X : BOOLEAN ;
    P1 : STRING ( 20 ) ;
    P2 : STRING ( 200 ) ;
    I : INTEGER ;
    TTAB : STRING ( 256 ) ;


const IDENT_T1 : TRANSLATE_TAB =
      ( X'00' , X'01' , X'02' , X'03' , X'04' , X'05' , X'06' , X'07' ,
        X'08' , X'09' , X'0a' , X'0b' , X'0c' , X'0d' , X'0e' , X'0f' ,
        X'10' , X'11' , X'12' , X'13' , X'14' , X'15' , X'16' , X'17' ,
        X'18' , X'19' , X'1a' , X'1b' , X'1c' , X'1d' , X'1e' , X'1f' ,
        X'20' , X'21' , X'22' , X'23' , X'24' , X'25' , X'26' , X'27' ,
        X'28' , X'29' , X'2a' , X'2b' , X'2c' , X'2d' , X'2e' , X'2f' ,
        X'30' , X'31' , X'32' , X'33' , X'34' , X'35' , X'36' , X'37' ,
        X'38' , X'39' , X'3a' , X'3b' , X'3c' , X'3d' , X'3e' , X'3f' ,
        X'40' , X'41' , X'42' , X'43' , X'44' , X'45' , X'46' , X'47' ,
        X'48' , X'49' , X'4a' , X'4b' , X'4c' , X'4d' , X'4e' , X'4f' ,
        X'50' , X'51' , X'52' , X'53' , X'54' , X'55' , X'56' , X'57' ,
        X'58' , X'59' , X'5a' , X'5b' , X'5c' , X'5d' , X'5e' , X'5f' ,
        X'60' , X'61' , X'62' , X'63' , X'64' , X'65' , X'66' , X'67' ,
        X'68' , X'69' , X'6a' , X'6b' , X'6c' , X'6d' , X'6e' , X'6f' ,
        X'70' , X'71' , X'72' , X'73' , X'74' , X'75' , X'76' , X'77' ,
        X'78' , X'79' , X'7a' , X'7b' , X'7c' , X'7d' , X'7e' , X'7f' )
        ;
      IDENT_T2 : TRANSLATE_TAB =
      ( X'80' , X'81' , X'82' , X'83' , X'84' , X'85' , X'86' , X'87' ,
        X'88' , X'89' , X'8a' , X'8b' , X'8c' , X'8d' , X'8e' , X'8f' ,
        X'90' , X'91' , X'92' , X'93' , X'94' , X'95' , X'96' , X'97' ,
        X'98' , X'99' , X'9a' , X'9b' , X'9c' , X'9d' , X'9e' , X'9f' ,
        X'a0' , X'a1' , X'a2' , X'a3' , X'a4' , X'a5' , X'a6' , X'a7' ,
        X'a8' , X'a9' , X'aa' , X'ab' , X'ac' , X'ad' , X'ae' , X'af' ,
        X'b0' , X'b1' , X'b2' , X'b3' , X'b4' , X'b5' , X'b6' , X'b7' ,
        X'b8' , X'b9' , X'ba' , X'bb' , X'bc' , X'bd' , X'be' , X'bf' ,
        X'c0' , X'c1' , X'c2' , X'c3' , X'c4' , X'c5' , X'c6' , X'c7' ,
        X'c8' , X'c9' , X'ca' , X'cb' , X'cc' , X'cd' , X'ce' , X'cf' ,
        X'd0' , X'd1' , X'd2' , X'd3' , X'd4' , X'd5' , X'd6' , X'd7' ,
        X'd8' , X'd9' , X'da' , X'db' , X'dc' , X'dd' , X'de' , X'df' ,
        X'e0' , X'e1' , X'e2' , X'e3' , X'e4' , X'e5' , X'e6' , X'e7' ,
        X'e8' , X'e9' , X'ea' , X'eb' , X'ec' , X'ed' , X'ee' , X'ef' ,
        X'f0' , X'f1' , X'f2' , X'f3' , X'f4' , X'f5' , X'f6' , X'f7' ,
        X'f8' , X'f9' , X'fa' , X'fb' , X'fc' , X'fd' , X'fe' , X'ff' )
        ;



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

   begin (* STRFUNC3 *)
     STRFUNC3 := X || '/' || Y ;
   end (* STRFUNC3 *) ;



function STRFUNC5 ( var X : STRING ; var Y : STRING ) : STRING ;

   begin (* STRFUNC5 *)
     STRFUNC5 := STRFUNC3 ( X , Y ) ;
   end (* STRFUNC5 *) ;



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



procedure TESTSTR ( X : STRING ( 20 ) ; var Y : STRING ; const Z :
                  STRING ) ;

   type LENGTHF = 0 .. 32767 ;

   var CP : -> CHAR ;
       LF : -> LENGTHF ;

   begin (* TESTSTR *)
     LF := ADDR ( X ) ;
     WRITELN ( 'lf 1 von x    = ' , LF -> ) ;
     LF := PTRADD ( LF , 2 ) ;
     WRITELN ( 'lf 2 von x    = ' , LF -> ) ;
     CP := PTRADD ( LF , 2 ) ;
     WRITELN ( '1. byte von x = ' , ORD ( CP -> ) ) ;
     WRITELN ( 'x [1]         = ' , X [ 1 ] ) ;
     WRITELN ( 'x [4]         = ' , X [ 4 ] ) ;

     //************************************************************
     // y genauer anschauen                                        
     //************************************************************

     LF := ADDR ( Y ) ;
     WRITELN ( 'lf 1 von y    = ' , LF -> ) ;
     LF := PTRADD ( LF , 2 ) ;
     WRITELN ( 'lf 2 von y    = ' , LF -> ) ;
     CP := PTRADD ( LF , 2 ) ;
     WRITELN ( '1. byte von y = ' , ORD ( CP -> ) ) ;
     WRITELN ( 'y [1]         = ' , Y [ 1 ] ) ;
     WRITELN ( 'y [4]         = ' , Y [ 4 ] ) ;
     Y := 'Oppolzer' ;
     Y [ 5 ] := 'S' ;

     //************************************************************
     // z genauer anschauen                                        
     //************************************************************

     LF := ADDR ( Z ) ;
     WRITELN ( 'lf 1 von z    = ' , LF -> ) ;
     LF := PTRADD ( LF , 2 ) ;
     WRITELN ( 'lf 2 von z    = ' , LF -> ) ;
     CP := PTRADD ( LF , 2 ) ;
     WRITELN ( '1. byte von z = ' , ORD ( CP -> ) ) ;
     WRITELN ( 'z [1]         = ' , Z [ 1 ] ) ;
     WRITELN ( 'z [4]         = ' , Z [ 4 ] ) ;

     //************************************************************
     // Z := 'Oppolzer' ;                                          
     // Z [ 5 ] := 'S' ;                                           
     //************************************************************

   end (* TESTSTR *) ;



function CHECKF : STRING ;

   var X : ANYPTR ;

   begin (* CHECKF *)
     X := STRRESULTP ;
     WRITELN ( 'strresultp      = ' , X ) ;
     WRITELN ( 'strresultp      = ' , STRRESULTP ) ;
     CHECKF := 'Bernd Oppolzer' ;
     WRITELN ( 'strresultp      = ' , STRRESULTP ) ;
     WRITELN ( 'length (result) = ' , LENGTH ( STRRESULT ) ) ;
     CHECKF := 'Bernd Oppolzer' || STRRESULT ;
     WRITELN ( 'strresultp      = ' , STRRESULTP ) ;
     WRITELN ( 'length (result) = ' , LENGTH ( STRRESULT ) ) ;
     CHECKF := REPEATSTR ( '*' , 1000 ) ;
     WRITELN ( 'strresultp      = ' , STRRESULTP ) ;
     WRITELN ( 'length (result) = ' , LENGTH ( STRRESULT ) ) ;
   end (* CHECKF *) ;



function TESTCONST ( const S : STRING ) : STRING ;

//**********************************************************************
// checkout problem with const strings                                  
//**********************************************************************


   var CP : -> CHAR ;

   begin (* TESTCONST *)
     TESTCONST := S ;
     CP := STRRESULTP ;
     CP -> := 'A' ;
   end (* TESTCONST *) ;



procedure TEST1 ;

   begin (* TEST1 *)
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

     //*************************************************************
     // ist nicht zulaessig, muss Konstante sein oder Funktion STR  
     //*************************************************************
     // VC20 := C20 ;  /* sollte Fehler sein */                     
     //*************************************************************
     // so korrekt                                                  
     //*************************************************************

     VC20 := STR ( C20 ) ;
     WRITE ( 'VC20 nach Zuweisung STR (C20) ........: ' ) ;
     WRITELN ( '<' , VC20 , '>' ) ;

     //*************************************************************
     // muss erlaubt sein, mit Blanks auffuellen wie gehabt ...     
     //*************************************************************

     VC20 := 'Bernd' ;
     WRITE ( 'VC20 nach Zuweisung Bernd ............: ' ) ;
     WRITELN ( '<' , VC20 , '>' ) ;
     C20 := VC20 ;
     WRITELN ( 'C20 nach Zuweisung VC20 ..............: ' , C20 ) ;
     C20 := 'Bernd ' || 'Oppolzer' ;
     WRITELN ( 'C20 nach Zuweisung String Expression .: ' , C20 ) ;

     //*************************************************************
     // Verkettung usw.                                             
     //*************************************************************

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
     WRITE ( 'VC200 nach Verkettung ................: ' ) ;
     WRITELN ( '<' , VC200 , '>' ) ;
     VC200 := ( VC20 || ' Oppolzer' ) || ' Leinfelden' ;
     WRITE ( 'VC200 nach Verkettung ................: ' ) ;
     WRITELN ( '<' , VC200 , '>' ) ;
     VC200 := ( ( VC20 || ' Oppolzer' ) || ' Leinfelden' ) ;
     WRITE ( 'VC200 nach Verkettung ................: ' ) ;
     WRITELN ( '<' , VC200 , '>' ) ;
     VC200 := VC20 || STR ( ' Oppolzer' ) || STR ( ' Leinfelden' ) ;
     WRITE ( 'VC200 nach Verkettung ................: ' ) ;
     WRITELN ( '<' , VC200 , '>' ) ;
     VC200 := VC20 || ' ' || VC20 ;
     WRITE ( 'VC200 nach Verkettung ................: ' ) ;
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
   end (* TEST1 *) ;



procedure TEST2 ;

   begin (* TEST2 *)

     //************************************************************
     // Zuweisungen einfach oder Expression                        
     //************************************************************

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

     //*************************************************************
     // Vergleiche von Strings                                      
     //*************************************************************

     X := VC20 = 'Bernd' ;
     WRITELN ( 'vc20 = ''Bernd'' .......................: ' , X ) ;
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
     WRITELN ( 'c20 > vc20 ...........................: ' , X ) ;
     X := VC20 > C20 ;
     WRITELN ( 'c20 < vc20 ...........................: ' , X ) ;
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

     //*************************************************************
     // verkettung direkt in Writeln ...                            
     //*************************************************************

     VC200 := VC20 || VC20 ;
     WRITE ( 'VC200 nach Verkettung ................: ' ) ;
     WRITELN ( '<' , VC200 , '>' ) ;
     WRITE ( 'Verkettung direkt ausgeben ...........: ' ) ;
     WRITELN ( '<' , VC20 || VC20 , '>' ) ;
     WRITE ( 'Verkettung nochmal direkt ausgeben ...: ' ) ;
     WRITELN ( '<' , VC20 || ' ' || C20A , '>' ) ;
   end (* TEST2 *) ;



procedure TEST3 ;

   begin (* TEST3 *)

     //************************************************************
     // tests mit prozeduren, call by value und by reference       
     //************************************************************

     WRITELN ( 'Aufruf STRPROC' ) ;
     STRPROC ( 'Bernd' , P1 , 'xxxxxxxxxxxxxxx' , P2 ) ;
     WRITELN ( 'zurÅck von STRPROC' ) ;
     WRITE ( 'P1 nach STRPROC ......................: ' ) ;
     WRITELN ( '<' , P1 , '>' ) ;
     WRITE ( 'P2 nach STRPROC ......................: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;

     //*************************************************************
     // noch ein Test, unpassende var-Parameter                     
     //*************************************************************

     WRITELN ( 'Aufruf STRPROC' ) ;
     STRPROC ( 'Bernd' , P2 , 'xxxxxxxxxxxxxxx' , P1 ) ;
     WRITELN ( 'zurÅck von STRPROC' ) ;
     WRITE ( 'P1 nach STRPROC ......................: ' ) ;
     WRITELN ( '<' , P1 , '>' ) ;
     WRITE ( 'P2 nach STRPROC ......................: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;

     //*************************************************************
     // length und maxlength ausprobieren                           
     //*************************************************************

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

     //*************************************************************
     // Funktion mit string ergebnis ausprobieren                   
     //*************************************************************

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
   end (* TEST3 *) ;



procedure TEST4 ;

   begin (* TEST4 *)

     //************************************************************
     // test Funktion mit const String parametern                  
     //************************************************************

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
     P1 := 'Oppolzer' ;
     P2 := STRFUNC2 ( VC20 || ' ' , ' Oppolzer' ) ;
     WRITE ( 'p2 = funktion mit const String .......: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;

     //*************************************************************
     // test SUBSTR1 - einfache Variante                            
     //*************************************************************

     P1 := 'Oppolzer' ;
     P2 := 'Oppolzer' ;
     TESTSTR ( P1 , P2 , 'Oppolzer' ) ;
     WRITE ( 'P1 nach Aufruf TESTSTR ...............: ' ) ;
     WRITELN ( '<' , P1 , '>' ) ;
     WRITE ( 'P2 nach Aufruf TESTSTR ...............: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;
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
     // ... geht nur in Funktion mit String Result !!               
     // WRITE ( 'Testausgabe STRRESULT ................: ' ) ;      
     // WRITELN ( STRRESULT ) ;                                     
     // WRITE ( 'Testausgabe STRRESULTP ...............: ' ) ;      
     // WRITELN ( STRRESULTP ) ;                                    
     //*************************************************************

     VC2000 := CHECKF ;
     WRITE ( 'Laenge des Ergebnisses von CHECKF ....: ' ) ;
     WRITELN ( LENGTH ( VC2000 ) ) ;
     WRITE ( 'Test REPEATSTR .......................: ' ) ;
     WRITELN ( REPEATSTR ( 'Bernd ' , 5 ) ) ;

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



procedure TEST5 ;

   begin (* TEST5 *)

     //************************************************************
     // test DELETE                                                
     //************************************************************

     P1 := 'Oppolzer' ;
     P2 := DELETE ( P1 , 3 , 4 ) ;
     WRITE ( 'p2 = Ergebnis von DELETE .............: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;
     WRITE ( 'P1 nach DELETE (should not change) ...: ' ) ;
     WRITELN ( '<' , P1 , '>' ) ;
     P2 := DELETE ( 'Bernd ' || 'Oppolzer' , 5 , 7 ) ;
     WRITE ( 'p2 = Ergebnis von DELETE .............: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;
     P2 := DELETE ( 'Bernd ' || 'Oppolzer' , 5 ) ;
     WRITE ( 'p2 = Ergebnis von DELETE .............: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;

     //*************************************************************
     // test RTRIM und LTRIM, TRIM und COMPRESS                     
     //*************************************************************

     P1 := '  Bernd Oppolzer    ' ;
     P2 := RTRIM ( P1 ) ;
     WRITE ( 'p2 = Ergebnis von RTRIM ..............: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;
     WRITE ( 'P1 nach RTRIM (should not change) ....: ' ) ;
     WRITELN ( '<' , P1 , '>' ) ;
     WRITE ( 'p2 = Ergebnis von RTRIM ..............: ' ) ;
     WRITELN ( '<' , RTRIM ( '  a  b  ' ) , '>' ) ;
     WRITE ( 'p2 = Ergebnis von RTRIM ..............: ' ) ;
     WRITELN ( '<' , RTRIM ( '    ' ) , '>' ) ;
     P1 := '  Bernd Oppolzer    ' ;
     P2 := LTRIM ( P1 ) ;
     WRITE ( 'p2 = Ergebnis von LTRIM ..............: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;
     WRITE ( 'P1 nach LTRIM (should not change) ....: ' ) ;
     WRITELN ( '<' , P1 , '>' ) ;
     WRITE ( 'p2 = Ergebnis von LTRIM ..............: ' ) ;
     WRITELN ( '<' , LTRIM ( '  a  b  ' ) , '>' ) ;
     WRITE ( 'p2 = Ergebnis von LTRIM ..............: ' ) ;
     WRITELN ( '<' , LTRIM ( '    ' ) , '>' ) ;
     P1 := '  Bernd Oppolzer    ' ;
     P2 := TRIM ( P1 ) ;
     WRITE ( 'p2 = Ergebnis von TRIM ...............: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;
     WRITE ( 'P1 nach TRIM (should not change) .....: ' ) ;
     WRITELN ( '<' , P1 , '>' ) ;
     WRITE ( 'p2 = Ergebnis von TRIM ...............: ' ) ;
     WRITELN ( '<' , TRIM ( '  a  b  ' ) , '>' ) ;
     WRITE ( 'p2 = Ergebnis von TRIM ...............: ' ) ;
     WRITELN ( '<' , TRIM ( '    ' ) , '>' ) ;
   end (* TEST5 *) ;



procedure TEST6 ;

   begin (* TEST6 *)

     //************************************************************
     // XP := RESULTP ;  ... not allowed here                      
     //************************************************************

     P1 := '  Bernd Oppolzer    ' ;
     P2 := COMPRESS ( P1 ) ;
     WRITE ( 'p2 = Ergebnis von COMPRESS ...........: ' ) ;
     WRITELN ( '<' , P2 , '>' ) ;
     WRITE ( 'P1 nach COMPRESS (should not change) .: ' ) ;
     WRITELN ( '<' , P1 , '>' ) ;
     WRITE ( 'p2 = Ergebnis von COMPRESS ...........: ' ) ;
     WRITELN ( '<' , COMPRESS ( '  a  b  ' ) , '>' ) ;
     WRITE ( 'p2 = Ergebnis von COMPRESS ...........: ' ) ;
     WRITELN ( '<' , COMPRESS ( '    ' ) , '>' ) ;

     //*************************************************************
     // test INDEX                                                  
     //*************************************************************

     P1 := '  Bernd Oppolzer    ' ;
     I := INDEX ( P1 , 'pol' ) ;
     WRITE ( 'Ergebnis von INDEX ...................: ' ) ;
     WRITELN ( I : 5 ) ;
     P1 := '  Bernd Oppolzer    ' ;
     I := INDEX ( TRIM ( P1 ) , 'pol' ) ;
     WRITE ( 'Ergebnis von INDEX ...................: ' ) ;
     WRITELN ( I : 5 ) ;
     WRITE ( 'P1 nach TRIM (should not change) .....: ' ) ;
     WRITELN ( '<' , P1 , '>' ) ;
     WRITE ( 'Ergebnis von INDEX (3) ...............: ' ) ;
     WRITELN ( INDEX ( 'Oppolzer' , 'pol' ) : 5 ) ;
     WRITE ( 'Ergebnis von INDEX (0) ...............: ' ) ;
     WRITELN ( INDEX ( 'Oppolzer' , 'polizei' ) : 5 ) ;
     WRITE ( 'Ergebnis von INDEX (1) ...............: ' ) ;
     WRITELN ( INDEX ( 'Oppolzer' , 'Oppolzer' ) : 5 ) ;
     WRITE ( 'Ergebnis von INDEX (0) ...............: ' ) ;
     WRITELN ( INDEX ( 'Oppolzer' , 'Oppolzer1' ) : 5 ) ;
     WRITE ( 'Ergebnis von INDEX (1) ...............: ' ) ;
     WRITELN ( INDEX ( 'Oppolzer' , 'Oppo' ) : 5 ) ;
     WRITE ( 'Ergebnis von INDEX (6) ...............: ' ) ;
     WRITELN ( INDEX ( 'Oppolzer' , 'zer' ) : 5 ) ;

     //*************************************************************
     // test VERIFY                                                 
     //*************************************************************

     P1 := '  Bernd Oppolzer    ' ;
     I := VERIFY ( P1 , 'Berndpol ' ) ;
     WRITE ( 'Ergebnis von VERIFY (9) ..............: ' ) ;
     WRITELN ( I : 5 ) ;
     P1 := '1256735473645' ;
     I := VERIFY ( P1 , '0123456789' ) ;
     WRITE ( 'Ergebnis von VERIFY (0) ..............: ' ) ;
     WRITELN ( I : 5 ) ;
     P1 := '125673547E645' ;
     I := VERIFY ( P1 , '0123456789' ) ;
     WRITE ( 'Ergebnis von VERIFY (10) .............: ' ) ;
     WRITELN ( I : 5 ) ;
     P1 := '125 73547E645' ;
     I := VERIFY ( P1 , '0123456789' ) ;
     WRITE ( 'Ergebnis von VERIFY (4) ..............: ' ) ;
     WRITELN ( I : 5 ) ;
     P1 := '1234567890123456789 ' ;
     I := VERIFY ( P1 , '0123456789' ) ;
     WRITE ( 'Ergebnis von VERIFY (20) .............: ' ) ;
     WRITELN ( I : 5 ) ;
   end (* TEST6 *) ;



begin (* HAUPTPROGRAMM *)
  TEST1 ;
  TEST2 ;
  TEST3 ;
  TEST4 ;
  TEST5 ;
  TEST6 ;

  //******************************************************************
  // test TRANSLATE                                                   
  //******************************************************************

  P1 := '  Bernd Oppolzer    ' ;
  WRITE ( 'P1 before testconst ..................: ' ) ;
  WRITELN ( '<' , P1 , '>' ) ;
  P2 := TESTCONST ( P1 ) ;
  WRITE ( 'P2 after testconst ...................: ' ) ;
  WRITELN ( '<' , P2 , '>' ) ;
  WRITE ( 'P1 after testconst (should not change): ' ) ;
  WRITELN ( '<' , P1 , '>' ) ;
  P1 := '  Bernd Oppolzer    ' ;
  P2 := TRANSLATE ( P1 , 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' ,
        'abcdefghijklmnopqrstuvwxyz' ) ;
  WRITE ( 'Ergebnis von TRANSLATE ...............: ' ) ;
  WRITELN ( '<' , P2 , '>' ) ;
  WRITE ( 'P1 nach TRANSLATE (should not change) : ' ) ;
  WRITELN ( '<' , P1 , '>' ) ;
  P1 := '  Bernd Oppolzer    ' ;
  P2 := TRANSLATE ( P1 , 'y' , 'z' ) ;
  WRITE ( 'Ergebnis von TRANSLATE ...............: ' ) ;
  WRITELN ( '<' , P2 , '>' ) ;
  WRITE ( 'P1 nach TRANSLATE (should not change) : ' ) ;
  WRITELN ( '<' , P1 , '>' ) ;
  P1 := '  Bernd Oppolzer    ' ;
  WRITE ( 'Ergebnis von TRANSLATE ...............: ' ) ;
  WRITELN ( '<' , TRANSLATE ( P1 , 'CAyx' , 'BOze' ) , '>' ) ;
  WRITE ( 'P1 nach TRANSLATE (should not change) : ' ) ;
  WRITELN ( '<' , P1 , '>' ) ;
  P1 := '  Bernd Oppolzer    ' ;
  WRITE ( 'Ergebnis von TRANSLATE ...............: ' ) ;
  WRITELN ( '<' , TRANSLATE ( P1 , 'x' , ' ' ) , '>' ) ;
  WRITE ( 'P1 nach TRANSLATE ....................: ' ) ;
  WRITELN ( '<' , P1 , '>' ) ;
  TTAB := IDENT_T1 || IDENT_T2 ;
  TTAB [ ORD ( ' ' ) + 1 ] := 'y' ;
  P1 := '  Bernd Oppolzer    ' ;
  WRITE ( 'Ergebnis von TRANSLATE ...............: ' ) ;
  WRITELN ( '<' , TRANSLATE ( P1 , TTAB ) , '>' ) ;
  WRITE ( 'P1 nach TRANSLATE (should not change) : ' ) ;
  WRITELN ( '<' , P1 , '>' ) ;

  //******************************************************************
  // Ende aller Tests                                                 
  //******************************************************************

end (* HAUPTPROGRAMM *) .
