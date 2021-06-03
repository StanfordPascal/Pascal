program MODIFY ( INP , OUT ) ;


var INP , OUT : TEXT ;
    ZEILE : STRING ( 72 ) ;
    ZEILIN : STRING ( 256 ) ;
    X : INTEGER ;
    FCB : VOIDPTR ;
    PFN : -> CHAR ;
    FN : CHAR ( 8 ) ;



function UPPER ( const S : STRING ) : STRING ;

   var I : INTEGER ;

   begin (* UPPER *)
     for I := 1 to LENGTH ( S ) do
       if S [ I ] in [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' ] then
         S [ I ] := CHR ( ORD ( S [ I ] ) - ORD ( 'a' ) + ORD ( 'A' ) )
                    ;
     UPPER := S ;
   end (* UPPER *) ;



begin (* HAUPTPROGRAMM *)
  RESET ( INP ) ;
  REWRITE ( OUT ) ;
  while not EOF ( INP ) do
    begin
      READLN ( INP , ZEILIN ) ;
      ZEILE := LEFT ( ZEILIN , 72 ) ;
      ZEILE := RTRIM ( ZEILE ) ;
      X := INDEX ( ZEILE , 'MNOTE *,' ) ;
      if X = 0 then
        WRITELN ( OUT , ZEILE )
      else
        begin
          FCB := FILEFCB ( INP ) ;
          PFN := PTRADD ( FCB , 17 ) ;
          MEMCPY ( ADDR ( FN ) , PFN , 8 ) ;
          FN := TRANSLATE ( FN , ' ' , CHR ( 0 ) ) ;
          FN := UPPER ( FN ) ;
          ZEILE := '         MNOTE *,''SP Macros - Macro ' || FN ||
                   ' - 2020.05 - Opp''' ;
          WRITELN ( OUT , ZEILE )
        end (* else *)
    end (* while *)
end (* HAUPTPROGRAMM *) .
