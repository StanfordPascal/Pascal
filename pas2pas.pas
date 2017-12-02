module $PAS2PAS ;

//**********************************************************************
//$A+
//**********************************************************************



type CHAR20 = array [ 1 .. 20 ] of CHAR ;



procedure PAS2PAS ( X1 : INTEGER ; var X2 : INTEGER ; T1 : CHAR20 ; var
                  T2 : CHAR20 ) ;

   begin (* PAS2PAS *)
     WRITELN ( 'pas2pas: x1 = ' , X1 ) ;
     WRITELN ( 'pas2pas: x2 = ' , X2 ) ;
     WRITELN ( 'pas2pas: t1 = <' , T1 , '>' ) ;
     WRITELN ( 'pas2pas: t2 = <' , T2 , '>' ) ;
     X2 := 5 ;
     T2 := 'string from pas2pas' ;
   end (* PAS2PAS *) ;



function PAS2Pf ( X1 : INTEGER ; x2 : INTEGER ) : integer ;

   begin (* PAS2pf *)
     pas2pf := ( x1 + x2 ) div 2 ;
   end (* PAS2PAS *) ;



begin (* HAUPTPROGRAMM *)

end (* HAUPTPROGRAMM *) .
