program TESTLEV ( OUTPUT ) ;

//**********************************************************************
// TEST VON STATISCHEN LEVELS                                           
// MIT MODULE TESTLEVU                                                  
//$A+                                                                   
//**********************************************************************




procedure LEVELU1 ( P : INTEGER ) ;

   EXTERNAL ;



procedure LEVEL1 ( P : INTEGER ) ;

   var X : INTEGER ;
       X1 : INTEGER ;


   procedure LEVEL2 ( P : INTEGER ) ;

      var X : INTEGER ;
          X2 : INTEGER ;


      procedure LEVEL3 ( P : INTEGER ) ;

         var X : INTEGER ;


         procedure LEVEL4 ( P : INTEGER ) ;

            var X : INTEGER ;


            procedure LEVEL5 ( P : INTEGER ) ;

               var X : INTEGER ;


               procedure LEVEL6 ( P : INTEGER ) ;

                  var X : INTEGER ;


                  procedure LEVEL7 ( P : INTEGER ) ;

                     var X : INTEGER ;


                     procedure LEVEL8 ( P : INTEGER ) ;

                        var X : INTEGER ;

                        begin (* LEVEL8 *)
                          X := P + 1 ;
                          WRITELN ( 'LEVEL8: ' , P , X1 , X2 ) ;
                          LEVELU1 ( ( X ) ) ;
                          WRITELN ( 'LEVEL8: ' , P , X1 , X2 ) ;
                        end (* LEVEL8 *) ;


                     begin (* LEVEL7 *)
                       X := P + 1 ;
                       WRITELN ( 'LEVEL7: ' , P , X1 , X2 ) ;
                       LEVEL8 ( X ) ;
                       WRITELN ( 'LEVEL7: ' , P , X1 , X2 ) ;
                     end (* LEVEL7 *) ;


                  begin (* LEVEL6 *)
                    X := P + 1 ;
                    WRITELN ( 'LEVEL6: ' , P , X1 , X2 ) ;
                    LEVEL7 ( X ) ;
                    WRITELN ( 'LEVEL6: ' , P , X1 , X2 ) ;
                  end (* LEVEL6 *) ;


               begin (* LEVEL5 *)
                 X := P + 1 ;
                 WRITELN ( 'LEVEL5: ' , P , X1 , X2 ) ;
                 LEVEL6 ( X ) ;
                 WRITELN ( 'LEVEL5: ' , P , X1 , X2 ) ;
               end (* LEVEL5 *) ;


            begin (* LEVEL4 *)
              X := P + 1 ;
              WRITELN ( 'LEVEL4: ' , P , X1 , X2 ) ;
              LEVEL5 ( X ) ;
              WRITELN ( 'LEVEL4: ' , P , X1 , X2 ) ;
            end (* LEVEL4 *) ;


         begin (* LEVEL3 *)
           X := P + 1 ;
           WRITELN ( 'LEVEL3: ' , P , X1 , X2 ) ;
           LEVEL4 ( X ) ;
           WRITELN ( 'LEVEL3: ' , P , X1 , X2 ) ;
         end (* LEVEL3 *) ;


      begin (* LEVEL2 *)
        X := P + 1 ;
        X2 := X ;
        WRITELN ( 'LEVEL2: ' , P , X1 , X2 ) ;
        LEVEL3 ( X ) ;
        WRITELN ( 'LEVEL2: ' , P , X1 , X2 ) ;
      end (* LEVEL2 *) ;


   begin (* LEVEL1 *)
     X := P + 1 ;
     X1 := X ;
     WRITELN ( 'LEVEL1: ' , P , X1 ) ;
     LEVEL2 ( X ) ;
     WRITELN ( 'LEVEL1: ' , P , X1 ) ;
   end (* LEVEL1 *) ;



begin (* HAUPTPROGRAMM *)
  WRITELN ( 'LEVEL0 ' ) ;
  LEVEL1 ( 1 ) ;
  WRITELN ( 'LEVEL0 ' ) ;
end (* HAUPTPROGRAMM *) .
