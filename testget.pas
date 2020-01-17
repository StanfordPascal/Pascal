program TESTGET ( INPUT , OUTPUT ) ;


var CH : CHAR ;



procedure TERMIN ( var X : TEXT ) ;

   EXTERNAL ;



procedure TERMOUT ( var X : TEXT ) ;

   EXTERNAL ;



begin (* HAUPTPROGRAMM *)
  TERMIN ( INPUT ) ;
  TERMOUT ( OUTPUT ) ;
  WRITELN ( 'termin, das Zeichen ist vor GET noch undefiniert' ) ;
  WRITELN ( 'wegen fehlendem Reset' ) ;
  CH := INPUT -> ;
  WRITELN ( '1. zeichen = <' , CH , '>' , ORD ( CH ) ) ;
  READ ( CH ) ;
  WRITELN ( 'nach read' ) ;
  WRITELN ( '1. zeichen = <' , CH , '>' , ORD ( CH ) ) ;
  CH := INPUT -> ;
  WRITELN ( '2. zeichen = <' , CH , '>' , ORD ( CH ) ) ;
  GET ;
  CH := INPUT -> ;
  WRITELN ( '3. zeichen = <' , CH , '>' , ORD ( CH ) ) ;
  GET ;
  CH := INPUT -> ;
  WRITELN ( '4. zeichen = <' , CH , '>' , ORD ( CH ) ) ;
end (* HAUPTPROGRAMM *) .
