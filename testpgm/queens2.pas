program QUEENS ( OUTPUT ) ;


const CMIN = 1 ;
      CMAX = 8 ;
      DMIN = 1 ;   // was - 7
      DMAX = 15 ;  // was 7


type CRANGE = CMIN .. CMAX ;
     COLUMN = set of CRANGE ;
     FIELD = array [ CRANGE ] of CRANGE ;
     DRANGE = DMIN .. DMAX ;
     DIAG = set of DRANGE ;


var QUEENS : FIELD ;
    SOLUTION : INTEGER ;



procedure PRINTQUEENS ;

   var ROW , COL : CRANGE ;

   begin (* PRINTQUEENS *)
     SOLUTION := SOLUTION + 1 ;
     WRITELN ( 'Solution ' , SOLUTION , ': ' ) ;
     for ROW := CMIN to CMAX do
       begin
         for COL := CMIN to CMAX do
           if QUEENS [ ROW ] = COL then
             WRITE ( 'Q' )
           else
             if ODD ( ROW + COL ) then
               WRITE ( ' ' )
             else
               WRITE ( '#' ) ;
         WRITELN
       end (* for *) ;
     WRITELN
   end (* PRINTQUEENS *) ;



procedure FINDQUEENS ( NEWQUEENROW : CRANGE ; COLS : COLUMN ; UPDIAG ,
                     DOWNDIAG : DIAG ) ;

   var TRIALCOL : CRANGE ;
       TRIALUP , TRIALDOWN : DRANGE ;

   begin (* FINDQUEENS *)
     for TRIALCOL := CMIN to CMAX do
       begin
         TRIALUP := NEWQUEENROW - TRIALCOL + 8 ;
         TRIALDOWN := NEWQUEENROW - ( CMAX + 1 - TRIALCOL ) + 8 ;
         if not ( TRIALCOL in COLS ) and not ( TRIALUP in UPDIAG ) and
         not ( TRIALDOWN in DOWNDIAG ) then
           begin
             QUEENS [ NEWQUEENROW ] := TRIALCOL ;
             if NEWQUEENROW < CMAX then
               FINDQUEENS ( NEWQUEENROW + 1 , COLS + [ TRIALCOL ] ,
                            UPDIAG + [ TRIALUP ] , DOWNDIAG + [
                            TRIALDOWN ] )
             else
               PRINTQUEENS
           end (* then *)
       end (* for *)
   end (* FINDQUEENS *) ;



begin (* HAUPTPROGRAMM *)
  SOLUTION := 0 ;
  FINDQUEENS ( CMIN , [ ] , [ ] , [ ] ) ;
  WRITELN ( 'Total solutions: ' , SOLUTION ) ;
end (* HAUPTPROGRAMM *) .
