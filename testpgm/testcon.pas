program TESTCON ;


type REC = record
             A : INTEGER ;
             B : INTEGER ;
             C : CHAR ( 20 ) ;
           end ;


var T1 : REC :=
         ( 12 , 15 , 'Bernd' ) ;



procedure TCON ( X : REC ; const Y : REC ; var Z : REC ) ;

   begin (* TCON *)
     WRITELN ( X . A , X . B , X . C ) ;
     WRITELN ( Y . A , Y . B , Y . C ) ;
     WRITELN ( Z . A , Z . B , Z . C ) ;
   end (* TCON *) ;



begin (* HAUPTPROGRAMM *)
  TCON ( T1 , T1 , T1 )
end (* HAUPTPROGRAMM *) .
