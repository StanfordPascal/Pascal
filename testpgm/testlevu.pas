module TESTLEVU ;

//**********************************************************************
// TEST VON STATISCHEN LEVELS                                           
//$A+                                                                   
//**********************************************************************




procedure LEVELU1 ( P : INTEGER ) ;

   var X : INTEGER ;
       U1 : INTEGER ;


   procedure LEVELU2 ( P : INTEGER ) ;

      var X : INTEGER ;
          U2 : INTEGER ;


      procedure LEVELU3 ( P : INTEGER ) ;

         var X : INTEGER ;


         procedure LEVELU4 ( P : INTEGER ) ;

            var X : INTEGER ;


            procedure LEVELU5 ( P : INTEGER ) ;

               var X : INTEGER ;


               procedure LEVELU6 ( P : INTEGER ) ;

                  var X : INTEGER ;


                  procedure LEVELU7 ( P : INTEGER ) ;

                     var X : INTEGER ;


                     procedure LEVELU8 ( P : INTEGER ) ;

                        var X : INTEGER ;

                        begin (* LEVELU8 *)
                          X := P + 1 ;
                          WRITELN ( 'LEVELU8:' , P , U1 , U2 ) ;
                        end (* LEVELU8 *) ;


                     begin (* LEVELU7 *)
                       X := P + 1 ;
                       WRITELN ( 'LEVELU7:' , P , U1 , U2 ) ;
                       LEVELU8 ( X ) ;
                       WRITELN ( 'LEVELU7:' , P , U1 , U2 ) ;
                     end (* LEVELU7 *) ;


                  begin (* LEVELU6 *)
                    X := P + 1 ;
                    WRITELN ( 'LEVELU6:' , P , U1 , U2 ) ;
                    LEVELU7 ( X ) ;
                    WRITELN ( 'LEVELU6:' , P , U1 , U2 ) ;
                  end (* LEVELU6 *) ;


               begin (* LEVELU5 *)
                 X := P + 1 ;
                 WRITELN ( 'LEVELU5:' , P , U1 , U2 ) ;
                 LEVELU6 ( X ) ;
                 WRITELN ( 'LEVELU5:' , P , U1 , U2 ) ;
               end (* LEVELU5 *) ;


            begin (* LEVELU4 *)
              X := P + 1 ;
              WRITELN ( 'LEVELU4:' , P , U1 , U2 ) ;
              LEVELU5 ( X ) ;
              WRITELN ( 'LEVELU4:' , P , U1 , U2 ) ;
            end (* LEVELU4 *) ;


         begin (* LEVELU3 *)
           X := P + 1 ;
           WRITELN ( 'LEVELU3:' , P , U1 , U2 ) ;
           LEVELU4 ( X ) ;
           WRITELN ( 'LEVELU3:' , P , U1 , U2 ) ;
         end (* LEVELU3 *) ;


      begin (* LEVELU2 *)
        X := P + 1 ;
        U2 := X ;
        WRITELN ( 'LEVELU2:' , P , U1 , U2 ) ;
        LEVELU3 ( X ) ;
        WRITELN ( 'LEVELU2:' , P , U1 , U2 ) ;
      end (* LEVELU2 *) ;


   begin (* LEVELU1 *)
     X := P + 1 ;
     U1 := X ;
     WRITELN ( 'LEVELU1:' , P , U1 ) ;
     LEVELU2 ( X ) ;
     WRITELN ( 'LEVELU1:' , P , U1 ) ;
   end (* LEVELU1 *) ;



begin (* HAUPTPROGRAMM *)
  
end (* HAUPTPROGRAMM *) .
