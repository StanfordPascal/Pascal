program TESTLEV ( OUTPUT ) ;

//**********************************************************************
// test von statischen levels                                           
//**********************************************************************




procedure LEVEL1 ( P : INTEGER ) ;

   var X : INTEGER ;


   procedure LEVEL2 ( P : INTEGER ) ;

      var X : INTEGER ;


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


                        procedure LEVEL9 ( P : INTEGER ) ;

                           var X : INTEGER ;


                           procedure LEVEL10 ( P : INTEGER ) ;

                              var X : INTEGER ;


                              procedure LEVEL11 ( P : INTEGER ) ;

                                 var X : INTEGER ;


                                 procedure LEVEL12 ( P : INTEGER ) ;

                                    var X : INTEGER ;

                                    begin (* LEVEL12 *)
                                      X := P + 1 ;
                                      WRITELN ( 'level12: ' , P ) ;
                                    end (* LEVEL12 *) ;


                                 begin (* LEVEL11 *)
                                   X := P + 1 ;
                                   WRITELN ( 'level11: ' , P ) ;
                                   LEVEL12 ( X ) ;
                                 end (* LEVEL11 *) ;


                              begin (* LEVEL10 *)
                                X := P + 1 ;
                                WRITELN ( 'level10: ' , P ) ;
                                LEVEL11 ( X ) ;
                              end (* LEVEL10 *) ;


                           begin (* LEVEL9 *)
                             X := P + 1 ;
                             WRITELN ( 'level9: ' , P ) ;
                             LEVEL10 ( X ) ;
                           end (* LEVEL9 *) ;


                        begin (* LEVEL8 *)
                          X := P + 1 ;
                          WRITELN ( 'level8: ' , P ) ;
                          LEVEL9 ( X ) ;
                        end (* LEVEL8 *) ;


                     begin (* LEVEL7 *)
                       X := P + 1 ;
                       WRITELN ( 'level7: ' , P ) ;
                       LEVEL8 ( X ) ;
                     end (* LEVEL7 *) ;


                  begin (* LEVEL6 *)
                    X := P + 1 ;
                    WRITELN ( 'level6: ' , P ) ;
                    LEVEL7 ( X ) ;
                  end (* LEVEL6 *) ;


               begin (* LEVEL5 *)
                 X := P + 1 ;
                 WRITELN ( 'level5: ' , P ) ;
                 LEVEL6 ( X ) ;
               end (* LEVEL5 *) ;


            begin (* LEVEL4 *)
              X := P + 1 ;
              WRITELN ( 'level4: ' , P ) ;
              LEVEL5 ( X ) ;
            end (* LEVEL4 *) ;


         begin (* LEVEL3 *)
           X := P + 1 ;
           WRITELN ( 'level3: ' , P ) ;
           LEVEL4 ( X ) ;
         end (* LEVEL3 *) ;


      begin (* LEVEL2 *)
        X := P + 1 ;
        WRITELN ( 'level2: ' , P ) ;
        LEVEL3 ( X ) ;
      end (* LEVEL2 *) ;


   begin (* LEVEL1 *)
     X := P + 1 ;
     WRITELN ( 'level1: ' , P ) ;
     LEVEL2 ( X ) ;
   end (* LEVEL1 *) ;



begin (* HAUPTPROGRAMM *)
  LEVEL1 ( 1 ) ;
end (* HAUPTPROGRAMM *) .
