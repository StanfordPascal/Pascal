program TESTP5 ( OUTPUT ) ;

//**********************************************************************
//$A+                                                                   
//**********************************************************************



var N : INTEGER ;
    X : STRING ( 25 ) ;


const DESC_CONST : array [ 1 .. 25 ] of STRING ( 25 ) =
      ( 'WIDGET-BLUE' , 'WIDGET-YELLOW' , 'WIDGET-GREEN' ,
        'WIDGET-PURPLE-N-GOLD' , 'WIDGET-BLUE' , 'WIDGET-ORANGE' ,
        'WIDGET-FANCY-RED' , 'WIDGET-FANCY-GREEN' ,
        'WIDGET-FANCY-YELLOW' , 'WIDGET-FANCY-ORANGE' ,
        'WIDGET-BLUE-WITH-SPARKLES' , 'WIDGET-RED-WITH-SPARKLES' ,
        'WIDGET-BLACK-CHERRY' , 'WIDGET-RED-N-GREEN' ,
        'WIDGET-LIGHTED-CLEAR' , 'ENDOFFILE' , 'RECORD 17' ,
        'RECORD 18' , 'RECORD 19' , 'RECORD 20' , 'RECORD 21' ,
        'RECORD 22' , 'RECORD 23' , 'RECORD 24' , 'RECORD 25' ) ;
      XCONST : STRING ( 25 ) = 'CONST1' ;
      ACONST : array [ 1 .. 2 ] of STRING ( 25 ) =
      ( 'C1' , 'C2' ) ;


begin (* HAUPTPROGRAMM *)
  X := 'Oppolzer' ;
  WRITELN ( X ) ;
  WRITELN ( XCONST ) ;
  WRITELN ( ACONST [ 1 ] ) ;
  if XCONST = 'ENDOF' then
    WRITELN ( 'XCONST = ''ENDOFFILE''' ) ;
  if ACONST [ 1 ] = 'ENDOFFILE' then
    WRITELN ( 'ACONST / 1 = ''ENDOFFILE''' ) ;
  WRITELN ( 'where is the string ''ENDOFFILE''?' ) ;
  for N := 1 to 25 do
    begin
      WRITELN ( N , ': ' , DESC_CONST [ N ] ) ;
      if DESC_CONST [ N ] = 'ENDOFFILE' then
        WRITELN ( 'string ''ENDOFFILE'' found at position ' , N ) ;
    end (* for *) ;
end (* HAUPTPROGRAMM *) .
