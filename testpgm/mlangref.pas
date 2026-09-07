module MLANGREF ;

(*************************************)
(*$A+,D-,N+ ... various options set  *)
(* nested comment (* pascal style *) *)
(*************************************)



const TAB_MAX = 1000 ;
      TAB_MAX2 : INTEGER = 1000 ;
      MESSAGE = 'This is a message' ;
      MESSAGE2 : CHAR ( 100 ) = 'This is a message' ;
      TAB : array [ 1 .. 5 ] of INTEGER =
      ( 10 , 20 , 30 , 40 , 50 ) ;


type DATE = record
              YEAR : 0 .. 9999 ;
              MONTH : 0 .. 12 ;
              DAY : 0 .. 31 ;
            end ;
     PERSON = record
                NAME : CHAR ( 30 ) ;
                BIRTH : DATE
              end ;


const X : PERSON =
      ( 'John' , ( 1957 , 7 , 4 ) ) ;


static CLONG : CHAR ( 32000 ) ;



procedure FILL_CLONG ( const X2 : CHAR ( 32000 ) ) ;

   var X : PERSON :=
           ( 'John' , ( 1957 , 7 , 4 ) ) ;
       I : INTEGER := 0 ;
       OK : BOOLEAN := TRUE ;

   begin (* FILL_CLONG *)
     MEMCPY ( ADDR ( CLONG ) , ADDR ( X2 ) , SIZEOF ( CLONG ) ) ;
   end (* FILL_CLONG *) ;



begin (* HAUPTPROGRAMM *)

end (* HAUPTPROGRAMM *) .
