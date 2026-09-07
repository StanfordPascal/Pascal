program QUEENS ( OUTPUT ) ;


const CMIN = 1 ;
      CMAX = 8 ;
      DMIN = - 7 ;
      DMAX = 7 ;  // was 7


type CRANGE = CMIN .. CMAX ;
     COLUMN = set of CRANGE ;
     FIELD = array [ CRANGE ] of CRANGE ;
     DRANGE = DMIN .. DMAX ;
     DIAG = set of DRANGE ;


var QUEENS : FIELD ;
    SOLUTION : INTEGER ;
    XCOLS : COLUMN :=
            [ ] ;
    XUPDIAG : DIAG :=
              [ ] ;
    XDOWNDIAG : DIAG :=
                [ ] ;



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



procedure FINDQUEENS ( NEWQUEENROW : CRANGE ;                      //
                     var COLS : COLUMN ;                           //
                     var UPDIAG , DOWNDIAG : DIAG ) ;              //

   var TRIALCOL : CRANGE ;
       TRIALUP , TRIALDOWN : DRANGE ;
       NCOLS : COLUMN ;
       NUPDIAG , NDOWNDIAG : DIAG ;

   begin (* FINDQUEENS *)
     for TRIALCOL := CMIN to CMAX do
       begin
         TRIALUP := NEWQUEENROW - TRIALCOL ;
         TRIALDOWN := NEWQUEENROW - ( CMAX + 1 - TRIALCOL ) ;
         if not ( TRIALCOL in COLS ) and not ( TRIALUP in UPDIAG ) and
         not ( TRIALDOWN in DOWNDIAG ) then
           begin
             QUEENS [ NEWQUEENROW ] := TRIALCOL ;
             if NEWQUEENROW < CMAX then
               begin
                 NCOLS := COLS + [ TRIALCOL ] ;
                 NUPDIAG := UPDIAG + [ TRIALUP ] ;
                 NDOWNDIAG := DOWNDIAG + [ TRIALDOWN ] ;
                 FINDQUEENS ( NEWQUEENROW + 1 , NCOLS , NUPDIAG ,
                              NDOWNDIAG )
               end (* then *)
             else
               PRINTQUEENS
           end (* then *)
       end (* for *)
   end (* FINDQUEENS *) ;



begin (* HAUPTPROGRAMM *)
  SOLUTION := 0 ;
  FINDQUEENS ( CMIN , XCOLS , XUPDIAG , XDOWNDIAG ) ;
  WRITELN ( 'Total solutions: ' , SOLUTION ) ;
end (* HAUPTPROGRAMM *) .
