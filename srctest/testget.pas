program TESTGET ( INPUT , OUTPUT ) ;


var CH : CHAR ;



procedure TERMIN ( var X : TEXT ) ;

   EXTERNAL ;



procedure TERMOUT ( var X : TEXT ) ;

   EXTERNAL ;



begin (* HAUPTPROGRAMM *)
  TERMIN ( INPUT ) ;

  //****************************************************************
  // termin does not do an implicit reset, only sets terminal flag  
  // input -> stays undefined (x'81') before the first get call     
  //****************************************************************
  // after that, get should return first char entered               
  //****************************************************************
  // the first get, in other words, does the same as                
  // an implicit reset (not really advancing to the second byte     
  // of the file)                                                   
  //****************************************************************

  GET ;
  while not EOF ( INPUT ) do
    begin
      CH := INPUT -> ;
      WRITELN ( 'x. zeichen = <' , CH , '>' , ORD ( CH ) ) ;
      GET ;
    end (* while *) ;
end (* HAUPTPROGRAMM *) .
