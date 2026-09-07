program QUEENS ( OUTPUT ) ;


const CMIN = 1 ;
      CMAX = 8 ;
      DMIN = - 7 ;
      DMAX = 7 ;  // was 7


static AUFRUFNR : INTEGER := 0 ;


type CRANGE = CMIN .. CMAX ;
     COLUMN = set of CRANGE ;
     FIELD = array [ CRANGE ] of CRANGE ;
     DRANGE = DMIN .. DMAX ;
     DIAG = set of DRANGE ;
     SHORT = - 10000 .. 10000 ;


var QUEENS : FIELD ;
    SOLUTION : INTEGER ;



procedure WRITE_SET ( S : set of - 10 .. 10 ; X : VOIDPTR ; SHOWDUMP :
                    BOOLEAN ) ;

   var I : INTEGER ;
       CP : -> CHAR ;
       SP : -> SHORT ;
       LEN : INTEGER ;

   begin (* WRITE_SET *)
     if SHOWDUMP then
       begin
         CP := PTRCAST ( ADDR ( S ) ) ;
         SP := PTRCAST ( ADDR ( S ) ) ;
         LEN := SP -> ;
         WRITE ( 'addr: ' , X , ' ' ) ;
         WRITE ( 'len: ' , LEN : 5 , ' ' ) ;
         SP := PTRADD ( SP , 2 ) ;
         WRITE ( 'offs: ' , SP -> : 5 , ' ' ) ;
         CP := PTRADD ( CP , 4 ) ;
         WRITE ( 'dump: ' ) ;
         for I := 1 to LEN do
           begin
             WRITE ( ORD ( CP -> ) : 1 , ' ' ) ;
             CP := PTRADD ( CP , 1 )
           end (* for *) ;
       end (* then *) ;
     WRITE ( 'inh.: ' ) ;
     for I := - 10 to 10 do
       if I in S then
         WRITE ( I : 1 , ' ' ) ;
     WRITELN ;
   end (* WRITE_SET *) ;



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
                     const COLS : COLUMN ;                         //
                     const UPDIAG , DOWNDIAG : DIAG ) ;            //

   var TRIALCOL : CRANGE ;
       TRIALUP , TRIALDOWN : DRANGE ;
       AUFNRLOK : INTEGER ;

   begin (* FINDQUEENS *)
     AUFRUFNR := AUFRUFNR + 1 ;
     AUFNRLOK := AUFRUFNR ;
     for TRIALCOL := CMIN to CMAX do
       begin
         TRIALUP := NEWQUEENROW - TRIALCOL ;
         TRIALDOWN := NEWQUEENROW - ( CMAX + 1 - TRIALCOL ) ;

     //***************************
     //  if AUFNRLOK < 1000 then
     //***************************

         if FALSE then
           begin
             WRITELN ( AUFNRLOK : 6 , ' check: ' , NEWQUEENROW : 2 ,
                       TRIALCOL : 2 ) ;
             WRITE ( ' cols: ' ) ;
             WRITE_SET ( COLS , ADDR ( COLS ) , TRUE ) ;
             WRITE ( ' updiag: ' ) ;
             WRITE_SET ( UPDIAG , ADDR ( UPDIAG ) , TRUE ) ;
             WRITE ( ' downdiag: ' ) ;
             WRITE_SET ( DOWNDIAG , ADDR ( DOWNDIAG ) , TRUE ) ;
             WRITELN
           end (* then *) ;
         if not ( TRIALCOL in COLS ) and not ( TRIALUP in UPDIAG ) and
         not ( TRIALDOWN in DOWNDIAG ) then
           begin
             QUEENS [ NEWQUEENROW ] := TRIALCOL ;
             if NEWQUEENROW < CMAX then
               begin
                 if FALSE then
                   begin
                     WRITELN ( AUFNRLOK : 6 , ' rekursiv weiter' ) ;
                     WRITE ( ' cols: ' ) ;
                     WRITE_SET ( COLS , ADDR ( COLS ) , TRUE ) ;
                     WRITE ( ' updiag: ' ) ;
                     WRITE_SET ( UPDIAG , ADDR ( UPDIAG ) , TRUE ) ;
                     WRITE ( ' downdiag: ' ) ;
                     WRITE_SET ( DOWNDIAG , ADDR ( DOWNDIAG ) , TRUE )
                                 ;
                     WRITELN
                   end (* then *) ;
                 FINDQUEENS ( NEWQUEENROW + 1 , COLS + [ TRIALCOL ] ,
                              UPDIAG + [ TRIALUP ] , DOWNDIAG + [
                              TRIALDOWN ] ) ;
                 if FALSE then
                   begin
                     WRITELN ( AUFNRLOK : 6 , ' rekursiv zurueck' ) ;
                     WRITE ( ' cols: ' ) ;
                     WRITE_SET ( COLS , ADDR ( COLS ) , TRUE ) ;
                     WRITE ( ' updiag: ' ) ;
                     WRITE_SET ( UPDIAG , ADDR ( UPDIAG ) , TRUE ) ;
                     WRITE ( ' downdiag: ' ) ;
                     WRITE_SET ( DOWNDIAG , ADDR ( DOWNDIAG ) , TRUE )
                                 ;
                     WRITELN
                   end (* then *) ;
               end (* then *)
             else
               begin
                 if FALSE then
                   WRITELN ( AUFNRLOK : 6 , ' treffer' ) ;
                 PRINTQUEENS
               end (* else *)
           end (* then *)
         else
           if FALSE then
             WRITELN ( AUFNRLOK : 6 , ' belegt' ) ;
       end (* for *)
   end (* FINDQUEENS *) ;



begin (* HAUPTPROGRAMM *)
  SOLUTION := 0 ;
  FINDQUEENS ( CMIN , [ ] , [ ] , [ ] ) ;
  WRITELN ( 'Total solutions: ' , SOLUTION ) ;
end (* HAUPTPROGRAMM *) .
