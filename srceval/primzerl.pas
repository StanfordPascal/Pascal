program PRIMZERL ( INPUT , OUTPUT ) ;


const TABSIZE = 100000 ;
      PRIMWIDTH = 8 ;
      PRIM_PER_LINE = 8 ;
      SHOW_TABLE_OF_PRIMES = FALSE ;


var PTAB : array [ 1 .. TABSIZE ] of INTEGER ;
    PLAUF : INTEGER ;
    PINDEX : INTEGER ;
    ZAHL , ZAHL1 , ZAHL2 : INTEGER ;
    PRIM_FOUND : BOOLEAN ;



function TESTPRIM ( TESTZAHL : INTEGER ; var IX : INTEGER ) : BOOLEAN ;

   var I : INTEGER ;
       REST : INTEGER ;

   begin (* TESTPRIM *)
     I := 2 ;
     TESTPRIM := FALSE ;
     while TRUE do
       begin
         if PTAB [ I ] * PTAB [ I ] > TESTZAHL then
           begin
             PTAB [ IX ] := TESTZAHL ;
             IX := IX + 1 ;
             TESTPRIM := TRUE ;
             break ;
           end (* then *) ;
         REST := TESTZAHL MOD PTAB [ I ] ;
         if REST = 0 then
           break ;
         I := I + 1 ;
       end (* while *)
   end (* TESTPRIM *) ;



procedure PRIMFAKTOR_ZERLEGUNG ( TESTZAHL : INTEGER ) ;

   var I : INTEGER ;
       REST : INTEGER ;
       ERSTE : BOOLEAN ;
       X : INTEGER ;

   begin (* PRIMFAKTOR_ZERLEGUNG *)
     ERSTE := TRUE ;
     I := 1 ;
     X := TESTZAHL ;
     while TRUE do
       begin
         if PTAB [ I ] * PTAB [ I ] > TESTZAHL then
           begin
             if ERSTE then
               WRITE ( TESTZAHL , ' ist Primzahl' )
             else
               WRITE ( X : 1 ) ;
             break ;
           end (* then *) ;
         REST := X MOD PTAB [ I ] ;
         if REST <> 0 then
           I := I + 1
         else
           begin
             if ERSTE then
               begin
                 WRITE ( TESTZAHL , ' = ' ) ;
                 ERSTE := FALSE
               end (* then *) ;
             X := X DIV PTAB [ I ] ;
             WRITE ( PTAB [ I ] : 1 ) ;
             if X = 1 then
               break ;
             WRITE ( ' * ' )
           end (* else *) ;
       end (* while *) ;
     WRITELN ;
   end (* PRIMFAKTOR_ZERLEGUNG *) ;



begin (* HAUPTPROGRAMM *)
  PTAB [ 1 ] := 2 ;
  PTAB [ 2 ] := 3 ;
  PTAB [ 3 ] := 5 ;
  PTAB [ 4 ] := 7 ;
  PTAB [ 5 ] := 11 ;
  PLAUF := 11 ;
  PINDEX := 6 ;
  if SHOW_TABLE_OF_PRIMES then
    WRITELN ( 'Tabelle der ersten ' , TABSIZE : 1 , ' Primzahlen' ) ;
  while PINDEX <= TABSIZE do
    begin
      PLAUF := PLAUF + 2 ;
      PRIM_FOUND := TESTPRIM ( PLAUF , PINDEX ) ;
      if PRIM_FOUND then
        if SHOW_TABLE_OF_PRIMES then
          begin
            if PINDEX MOD PRIM_PER_LINE = 1 then
              WRITELN ;
            WRITE ( PTAB [ PINDEX - 1 ] : PRIMWIDTH ) ;
          end (* then *)
        else
          if PINDEX MOD 5000 = 0 then
            WRITELN ( PINDEX , ' Primzahlen bereits gefunden' ) ;
    end (* while *) ;
  if SHOW_TABLE_OF_PRIMES then
    WRITELN ;
  while TRUE do
    begin
      WRITELN ;
      WRITELN ( 'Bitte Testzahlen von ... bis eingeben:' ) ;
      READLN ( ZAHL1 , ZAHL2 ) ;
      if ( ZAHL1 = 0 ) & ( ZAHL2 = 0 ) then
        break ;
      for ZAHL := ZAHL1 to ZAHL2 do
        begin
          PRIMFAKTOR_ZERLEGUNG ( ZAHL ) ;
        end (* for *) ;
      if EOF ( INPUT ) then
        break ;
    end (* while *) ;
end (* HAUPTPROGRAMM *) .
