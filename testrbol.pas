program TESTTBOL ( INPUT , OUTPUT ) ;


type X = array [ 1 .. 10 ] of INTEGER ;


var B : BOOLEAN ;
    S : STRING ( 50 ) ;
    F : ( ROT , GELB , GRUEN , BLAU ) ;
    A : array [ 1 .. 10 ] of INTEGER ;
    A2 : X ;
    I : INTEGER ;
    R : REAL ;
    CA : CHAR ( 7 ) := ' test ' ;
    CB : CHAR ( 7 ) := 'abcdefg' ;
    SA : STRING ( 20 ) ;



procedure TEST1 ;

   begin (* TEST1 *)
     S := 'F' ;
     READSTR ( S , B ) ;
     WRITELN ( 'String = ' , S ) ;
     WRITELN ( B ) ;
     S := 'rot' ;
     READSTR ( S , F ) ;
     WRITELN ( 'String = ' , S ) ;
     WRITELN ( F ) ;
     WRITELN ( 'test: ' , 4 : 5 , '<' ) ;
     WRITELN ( 'test: ' , 4 : - 5 , '<' ) ;
     WRITELN ( 'test: ' , - 4 : 5 , '<' ) ;
     WRITELN ( 'test: ' , - 4 : - 5 , '<' ) ;
     WRITELN ( 'test: ' , 1234567 : 5 , '<' ) ;
     WRITELN ( 'test: ' , - 1234567 : - 5 , '<' ) ;
     WRITELN ( 'test: ' , '*' : 5 ) ;
     WRITELN ( 'test: ' , '*' : 0 ) ;
     WRITELN ( 'test: ' , TRUE ) ;
     WRITELN ( 'test: ' , TRUE : 1 ) ;
     WRITELN ( 'test: ' , TRUE : 3 ) ;
     WRITELN ( 'test: ' , TRUE : 7 ) ;
     WRITELN ( 'test: ' , TRUE : - 7 ) ;
     WRITELN ( 'test: ' , TRUE : 0 ) ;
     I := 4 ;
     WRITESTR ( S , 4 : 5 , 4 : - 5 , - 4 : 5 , - 4 : - 5 , 1234567 : 5
                , - 1234567 : - 5 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , 4 : 5 , I : 5 , I + 2 : 7 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , 'X' ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , '*' : 12 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , 4 : 5 , '*' , I : - 5 , '*' , - I : - 5 , '*' ,
                1234567 : 5 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     if FALSE then
       WRITESTR ( S , 4 : 5 , I : - 5 , - I : - 5 , 1234567 : 55 ) ;

     //**********************
     // test mit char array  
     //**********************

     WRITELN ( 'test: ' , 'abstand' : 25 , '<<<' ) ;
     WRITELN ( 'test: ' , 'abstand' : 5 , '<<<' ) ;
     WRITELN ( 'test: ' , 'abstand' : - 25 , '<<<' ) ;
     WRITELN ( 'test: ' , 'abstand' : 0 , '<<<' ) ;

     //***********************
     // korrekt mit writestr  
     //***********************

     WRITESTR ( S , 4 : 5 , ' abstand ' , I : - 5 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , 4 : 5 , CA : 12 , I : - 5 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , 4 : 5 , CA : - 12 , I : - 5 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , 4 : 5 , CB : 12 , I : - 5 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , 4 : 5 , CB : - 12 , I : - 5 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;

     //***************************************
     // testen writestr mit boolean           
     //***************************************

     B := TRUE ;
     WRITESTR ( S , '>>>' , B , '<<<' ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , '>>>' , B : 1 , '<<<' ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , '>>>' , B : 3 , '<<<' ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , '>>>' , B : 7 , '<<<' ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , '>>>' , B : - 7 , '<<<' ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , '>>>' , B : 0 , '<<<' ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     B := FALSE ;
     WRITESTR ( S , '>>>' , B , '<<<' ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , '>>>' , B : 1 , '<<<' ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , '>>>' , B : 3 , '<<<' ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , '>>>' , B : 7 , '<<<' ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , '>>>' , B : - 7 , '<<<' ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , '>>>' , B : 0 , '<<<' ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
   end (* TEST1 *) ;



procedure TEST2 ;

   begin (* TEST2 *)

     //************************
     // test mit strings     
     //**********************
     //************************

     R := 123.456 ;
     SA := 'Bernd Oppolzer' ;
     WRITELN ( '>>>' , SA , '<<<' ) ;
     WRITELN ( '>>>' , SA : 23 , '<<<' ) ;
     WRITELN ( '>>>' , SA : - 23 , '<<<' ) ;
     WRITELN ( '>>>' , SA : 0 , '<<<' ) ;
     WRITESTR ( S , '>>>' , SA : 25 , '<<<' ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , '>>>' , SA : - 25 , '<<<' ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , '>>>' , SA : 9 , '<<<' ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , '>>>' , SA : - 9 , '<<<' ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , '>>>' , SA : 0 , '<<<' ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , '>>>' , 'Bernd' || ' Opp' , '<<<' ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'Test Write Real: ' , R : 10 : 2 ) ;
     R := 0.0 ;
     WRITESTR ( S , R : 10 : 2 , ' abstand ' , I : 5 , ' ' , B ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , R : - 10 : 2 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , R : 10 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , R : - 10 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , R ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     R := 123.456 ;
     I := 450000000 ;
     WRITESTR ( S , I : 9 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , I : - 9 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     I := - 450000000 ;
     WRITESTR ( S , I : 9 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , I : - 9 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     I := 45000000 ;
     WRITESTR ( S , I : 9 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , I : - 9 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     I := - 45000000 ;
     WRITESTR ( S , I : 9 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , I : - 9 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , R : 10 : 2 , ' abstand ' , I : 5 , ' ' , B ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , R : - 10 : 2 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , R : 10 : 0 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , R : - 10 : 0 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITELN ( 'Test Write Integer: ' , 5 : - 9 ) ;
     WRITELN ( 'Test Write Integer: ' , 0 : - 9 ) ;
     WRITESTR ( S , 5 : - 9 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , 0 : - 9 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , R : 10 : - 1 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , R : - 10 : - 1 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , R : 10 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , R : - 10 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , R ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     WRITESTR ( S , R : 30 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     R := - 12345.67890 ;
     WRITESTR ( S , R : 30 ) ;
     WRITELN ( 's nach writestr: >' , S , '<' ) ;
     EXIT ( 8 ) ;
     WRITESTR ( S , 'jetzt gibt''s ein Problem: ' , I : 15 ) ;
     A := A2 ;
   end (* TEST2 *) ;



begin (* HAUPTPROGRAMM *)
  TEST1 ;
  TEST2
end (* HAUPTPROGRAMM *) .
