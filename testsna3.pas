program TESTSNAP ( OUTPUT ) ;

(********)
(*$A+   *)
(********)



type POS_INT = 0 .. 30 ;


var I : POS_INT ;
    TIME : INTEGER ;


static TESTDUMP : INTEGER ;
       TESTCHAR : array [ 1 .. 10 ] of CHAR ;



function FIBONACCI ( J : POS_INT ) : INTEGER ;

(******************************)
(* to evaluate fibonacci # j, *)
(* for j >= 0                 *)
(* subject to int overflow    *)
(******************************)


   static ANZCALL : INTEGER ;

   var CP : -> CHAR ;
       C : CHAR ;

   begin (* FIBONACCI *)
     ANZCALL := ANZCALL + 1 ;
     if J = 0 then
       FIBONACCI := 0
     else
       if J = 1 then
         FIBONACCI := 1
       else
         FIBONACCI := FIBONACCI ( J - 1 ) + FIBONACCI ( J - 3 ) ;
   end (* FIBONACCI *) ;



begin (* HAUPTPROGRAMM *)
  TESTDUMP := 42 ;
  TESTCHAR := 'Oppolzer' ;
  for I := 10 to 25 do
    begin
      TIME := CLOCK ( 0 ) ;
      WRITELN ( ' fibonacci # ' , I : 3 , ' is ' , FIBONACCI ( I ) : 8
                , ' (Comp.time = ' , CLOCK ( 0 ) - TIME : 5 ,
                ' Milli Sec.)' ) ;
    end (* for *)
end (* HAUPTPROGRAMM *) .
