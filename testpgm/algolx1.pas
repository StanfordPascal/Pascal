program X1_ALGOL_60_COMPILER ( INPUT , OUTPUT , LIB_TAPE ) ;


const D2 = 4 ;
      D3 = 8 ;
      D4 = 16 ;
      D5 = 32 ;
      D6 = 64 ;
      D7 = 128 ;
      D8 = 256 ;
      D10 = 1024 ;
      D12 = 4096 ;
      D13 = 8192 ;
      D15 = 32768 ;
      D16 = 65536 ;
      D17 = 131072 ;
      D18 = 262144 ;
      D19 = 524288 ;
      D20 = 1048576 ;
      D21 = 2097152 ;
      D22 = 4194304 ;
      D23 = 8388608 ;
      D24 = 16777216 ;
      D25 = 33554432 ;
      D26 = 67108864 ;
      MZ = 134217727 ;
      GVC0 = 138 ;         (*0-04-10*)
      TLIB = 800 ;         (*0-25-00*)
      PLIE = 6783 ;        (*6-19-31*)
      BIM = 930 ;          (*0-29-02*)
      NLSCOP = 31 ;
      NLSC0 = 48 ;
      MLIB = 800 ;         (*0-25-00*)
      KLIE = 10165 ;       (*9-29-21*)
      CRFB = 623 ;         (*0-19-15*)
      MCPB = 928 ;         (*0-29-00*)


var TLSC , PLIB , FLIB , KLIB , NLIB , RHT , VHT , QC , SCAN , RFSB ,
    RNSA , RNSB , RNSC , RNSD , DL , INW , FNW , DFLAG , BFLAG , OFLAG
    , NFLAG , KFLAG , IFLAG , MFLAG , VFLAG , AFLAG , SFLAG , EFLAG ,
    JFLAG , PFLAG , FFLAG , BN , VLAM , PNLV , GVC , LVC , OH , ID ,
    NID , IBD , INBA , FORA , FORC , PSTA , PSTB , SPE , ARRA , ARRB ,
    ARRC , ARRD , IC , AIC , RLAA , RLAB , QA , QB , RLSC , FLSC , KLSC
    , NLSC : INTEGER ;
    BITCOUNT , BITSTOCK : INTEGER ;
    STORE : array [ 0 .. 12287 ] of INTEGER ;
    RNS_STATE : ( PS , MS , VIRGINAL ) ;
    RFS_CASE , NAS_STOCK , POS : INTEGER ;
    WORD_DEL_TABLE : array [ 10 .. 38 ] of INTEGER ;
    FLEX_TABLE : array [ 0 .. 127 ] of INTEGER ;
    OPC_TABLE : array [ 0 .. 112 ] of INTEGER ;
    RLIB , MCPE : INTEGER ;
    LIB_TAPE : TEXT ;
    II : INTEGER ;



procedure STOP ( N : INTEGER ) ;

(************************************)
(*emulation of a machine instruction*)
(************************************)


   begin (* STOP *)
     WRITELN ( OUTPUT ) ;
     WRITELN ( OUTPUT , '*** stop ' , N DIV D5 : 1 , '-' , N MOD D5 : 2
               , ' ***' ) ;
     EXIT ( 8 ) /* halt */
   end (* STOP *) ;



function READ_FLEXOWRITER_SYMbol : INTEGER ;                          (*LK*)

   label 1 , 2 ;

   var S , FTS : INTEGER ;

   begin (* READ_FLEXOWRITER_SYM *)
     1 :
     READ ( INPUT , S ) ;
     if RFSB = 0 then
       if ( S = 62  (*tab*) ) or ( S = 16  (*space*) ) or ( S = 26

     (******)
     (*crlf*)
     (******)

       ) then
         goto 2
       else
         if ( S = 122  (*lc*) ) or ( S = 124  (*uc*) ) or ( S = 0

     (*******)
     (*blank*)
     (*******)

         ) then
           begin
             RFSB := S  (*new flexowriter shift*) ;
             goto 1
           end (* then *)
         else
           if S = 127  (*erase*) then
             goto 1
           else
             STOP ( 19 )  (*flexowriter shift undefined*) ;
     2 :
     FTS := FLEX_TABLE [ S ] ;
     if FTS > 0 then
       if RFSB = 124 then  (*uppercase*)
         READ_FLEXOWRITER_SYMbol := FTS DIV D8
       else  (*lowercase*)
         READ_FLEXOWRITER_SYMbol := FTS MOD D8
     else
       if FTS = - 0 then
         STOP ( 20 )                  (*wrong parity*)
       else
         if FTS = - 1 then
           STOP ( 21 )                (*undefined punching*)
         else
           if S = 127  (*erase*) then
             goto 1
           else
             begin
               RFSB := S  (*new flexowriter shift*) ;
               goto 1
             end (* else *)
   end (* READ_FLEXOWRITER_SYM *) ;



function NEXT_ALGOL_SYMBOL : INTEGER ;                                (*HT*)

   label 1 ;

   var SYM , WDT1 , WDT2 : INTEGER ;

   begin (* NEXT_ALGOL_SYMBOL *)
     SYM := - NAS_STOCK ;
     if SYM >= 0 (*symbol in stock*)
     then
       NAS_STOCK := SYM + 1 (*stock empty now*)
     else
       SYM := READ_FLEXOWRITER_SYMbol ;
     1 :
     if SYM > 101 (*analysis required*)
     then
       begin
         if SYM = 123  (*space symbol*) then
           SYM := 93 ;
         if SYM <= 119   (*space symbol, tab, or nlcr*)
         then
           if QC = 0 then
             begin
               SYM := READ_FLEXOWRITER_SYMbol ;
               goto 1
             end (* then *)
           else
             
         else
           if SYM = 124      (*:*)
           then
             begin
               SYM := READ_FLEXOWRITER_SYMbol ;
               if SYM = 72 then
                 SYM := 92           (*:=*)
               else
                 begin
                   NAS_STOCK := - SYM ;
                   SYM := 90  (*:*)
                 end (* else *)
             end (* then *)
           else
             if SYM = 162    (*|*)
             then
               begin
                 repeat
                   SYM := READ_FLEXOWRITER_SYMbol
                 until SYM <> 162 ;
                 if SYM = 77  (*^*) then
                   SYM := 69                         (*|^*)
                 else
                   if SYM = 72  (*=*) then
                     SYM := 75                       (*|=*)
                   else
                     if SYM = 74  (*<*) then
                       SYM := 102                    (*|<*)
                     else
                       if SYM = 70  (*>*) then
                         SYM := 103                  (*|>*)
                       else
                         STOP ( 11 )
               end (* then *)
             else
               if SYM = 163  (*_*)
               then
                 begin
                   repeat
                     SYM := READ_FLEXOWRITER_SYMbol
                   until SYM <> 163 ;
                   if ( SYM > 9 ) and ( SYM <= 38 ) (*a..B*)
                   then
                     begin                         (*word delimiter*)
                       WDT1 := WORD_DEL_TABLE [ SYM ] MOD 128 ;
                       if WDT1 >= 63 then
                         SYM := WDT1
                       else
                         if WDT1 = 0 then
                           STOP ( 13 )
                         else
                           if WDT1 = 1      (*sym = c*)
                           then
                             if QC = 0      (*outside string*)
                             then
                               begin        (*skip comment*)
                                 repeat
                                   SYM := READ_FLEXOWRITER_SYMbol
                                 until SYM = 91  (*;*) ;
                                 SYM := READ_FLEXOWRITER_SYMbol ;
                                 goto 1
                               end (* then *)
                             else
                               SYM := 97   (*comment*)
                           else
                             begin
                               SYM := READ_FLEXOWRITER_SYMbol ;
                               if SYM = 163    (*_*)
                               then
                                 begin
                                   repeat
                                     SYM := READ_FLEXOWRITER_SYMbol
                                   until SYM <> 163 ;
                                   if ( SYM > 9 ) and ( SYM <= 32 )
                                   then
                                     if SYM = 29          (*t*)
                                     then
                                       begin
                                         SYM := READ_FLEXOWRITER_SYMbol
                                                ;
                                         if SYM = 163          (*_*)
                                         then
                                           begin
                                             repeat
                                               SYM :=
                                               READ_FLEXOWRITER_SYMbol
                                             until SYM <> 163 ;
                                             if SYM = 14             (*e*)
                                             then
                                               SYM := 94             (*step*)
                                             else
                                               SYM := 113            (*string*)
                                           end (* then *)
                                         else
                                           STOP ( 12 )
                                       end (* then *)
                                     else
                                       begin
                                         WDT2 := WORD_DEL_TABLE [ SYM ]
                                                 DIV 128 ;
                                         if WDT2 = 0 then
                                           SYM := WDT1 + 64
                                         else
                                           SYM := WDT2
                                       end (* else *)
                                   else
                                     STOP ( 13 )
                                 end (* then *)
                               else
                                 STOP ( 12 )
                             end (* else *) ;
                       repeat
                         NAS_STOCK := - READ_FLEXOWRITER_SYMbol ;
                         if NAS_STOCK = - 163     (*_*)
                         then
                           repeat
                             NAS_STOCK := READ_FLEXOWRITER_SYMbol
                           until NAS_STOCK <> 163
                       until NAS_STOCK <= 0
                     end (* then *)
                   else
                     if SYM = 70  (*>*) then
                       SYM := 71                       (*>=*)
                     else
                       if SYM = 72  (*=*) then
                         SYM := 80                     (*eqv*)
                       else
                         if SYM = 74  (*<*) then
                           SYM := 73                   (*<=*)
                         else
                           if SYM = 76  (*~*) then
                             SYM := 79                 (*imp*)
                           else
                             if SYM = 124  (*:*) then
                               SYM := 68               (*div*)
                             else
                               STOP ( 13 )
                 end (* then *)
               else
                 STOP ( 14 ) (*? or " or '*)
       end (* then *) ;
     NEXT_ALGOL_SYMBOL := SYM
   end (* NEXT_ALGOL_SYMBOL *) ;



procedure READ_NEXT_SYMBOL ;                                          (*ZY*)

   label 1 ;

   begin (* READ_NEXT_SYMBOL *)
     1 :
     case RNS_STATE of
       PS : begin
              DL := NEXT_ALGOL_SYMBOL ;

     (*******************************)
     (*store symbol in symbol store:*)
     (*******************************)

              if RNSA > D7 then
                begin
                  RNSA := RNSA DIV D7 ;
                  STORE [ RNSB ] := STORE [ RNSB ] + DL * RNSA
                end (* then *)
              else
                begin
                  RNSA := D15 ;
                  RNSB := RNSB + 1 ;
                  STORE [ RNSB ] := DL * RNSA ;
                  if RNSB + 8 > PLIB then
                    STOP ( 25 )
                end (* else *)
            end (* tag/ca *) ;
       MS : begin (*take symbol from symbol store:*)
              DL := ( STORE [ RNSD ] DIV RNSC ) MOD D7 ;
              if RNSC > D7 then
                RNSC := RNSC DIV D7
              else
                begin
                  RNSC := D15 ;
                  RNSD := RNSD + 1
                end (* else *)
            end (* tag/ca *) ;
       VIRGINAL :
         begin
           QC := 0 ;
           RFS_CASE := 0 ;
           NAS_STOCK := 1 ;
           if SCAN > 0 (*prescan*)
           then
             begin
               RNS_STATE := PS ;

     (**************************)
     (*initialize symbol store:*)
     (**************************)

               RNSB := BIM + 8 ;
               RNSD := BIM + 8 ;
               RNSA := D22 ;
               RNSC := D15 ;
               STORE [ RNSB ] := 0 ;
             end (* then *)
           else
             RNS_STATE := MS ;
           goto 1
         end (* tag/ca *)
     end (* case *)
   end (* READ_NEXT_SYMBOL *) ;



procedure READ_UNTIL_NEXT_DELImiter ;                                 (*FT*)

   label 1 , 3 , 4 , 5 ;

   var MARKER , ELSC , BEXP : INTEGER ;


   function TEST1 : BOOLEAN ;

      begin (* TEST1 *)
        if DL = 88   (*.*)
        then
          begin
            DFLAG := 1 ;
            READ_NEXT_SYMBOL ;
            TEST1 := TEST1
          end (* then *)
        else
          if DL = 89  (*ten*) then
            goto 1
          else
            TEST1 := DL > 9
      end (* TEST1 *) ;


   function TEST2 : BOOLEAN ;

      begin (* TEST2 *)
        if DL = 89  (*ten*) then
          INW := 1 ;
        TEST2 := TEST1
      end (* TEST2 *) ;


   function TEST3 : BOOLEAN ;

      begin (* TEST3 *)
        READ_NEXT_SYMBOL ;
        TEST3 := TEST1
      end (* TEST3 *) ;


   begin (* READ_UNTIL_NEXT_DELI *)
     READ_NEXT_SYMBOL ;
     NFLAG := 1 ;
     if ( DL > 9 ) and ( DL < 63 ) (*letter*)
     then
       begin
         DFLAG := 0 ;
         KFLAG := 0 ;
         INW := 0 ;
         repeat
           FNW := ( INW MOD D6 ) * D21 ;
           INW := INW DIV D6 + DL * D21 ;
           READ_NEXT_SYMBOL
         until ( INW MOD D3 > 0 ) or ( DL > 62 ) ;
         if INW MOD D3 > 0 then
           begin
             DFLAG := 1 ;
             FNW := FNW + D23 ;
             MARKER := 0 ;
             while ( MARKER = 0 ) and ( DL < 63 ) do
               begin
                 MARKER := FNW MOD D6 * D21 ;
                 FNW := FNW DIV 64 + DL * D21 ;
                 READ_NEXT_SYMBOL
               end (* while *) ;
             while MARKER = 0 do
               begin
                 MARKER := FNW MOD D6 * D21 ;
                 FNW := FNW DIV D6 + 63 * D21
               end (* while *) ;
             while DL < 62 do
               READ_NEXT_SYMBOL
           end (* then *) ;
         goto 4 ;
       end (* then *) ;
     KFLAG := 1 ;
     FNW := 0 ;
     INW := 0 ;
     DFLAG := 0 ;
     ELSC := 0 ;
     if TEST2 (*not (dl in [0..9,88,89])*)
     then
       begin
         NFLAG := 0 ;
         if ( DL = 116  (*true*) ) or ( DL = 117  (*false*) ) then
           begin
             INW := DL - 116 ;
             DFLAG := 0 ;
             KFLAG := 1 ;
             NFLAG := 1 ;
             READ_NEXT_SYMBOL ;
             goto 4
           end (* then *) ;
         goto 5
       end (* then *) ;
     repeat
       if FNW < D22 then
         begin
           INW := 10 * INW + DL ;
           FNW := 10 * FNW + INW DIV D26 ;
           INW := INW MOD D26 ;
           ELSC := ELSC - DFLAG
         end (* then *)
       else
         ELSC := ELSC - DFLAG + 1
     until TEST3 ;
     if ( DFLAG = 0 ) and ( FNW = 0 ) then
       goto 4 ;
     goto 3 ;
     1 :
     if TEST3 (*not (dl in [0..9,88,89]*)
     then
       if DL = 64 (*plus*)
       then
         begin
           READ_NEXT_SYMBOL ;
           DFLAG := DL
         end (* then *)
       else
         begin
           READ_NEXT_SYMBOL ;
           DFLAG := - DL - 1
         end (* else *)
     else
       DFLAG := DL ;
     while not TEST3  (*dl in [0..9,88,89]*) do
       begin
         if DFLAG >= 0 then
           DFLAG := 10 * DFLAG + DL
         else
           DFLAG := 10 * DFLAG - DL + 9 ;
         if ABS ( DFLAG ) >= D26 then
           STOP ( 3 )
       end (* while *) ;
     if DFLAG < 0 then
       DFLAG := DFLAG + 1 ;
     ELSC := ELSC + DFLAG ;
     3 : (*float*)
     if ( INW = 0 ) and ( FNW = 0 ) then
       begin
         DFLAG := 0 ;
         goto 4
       end (* then *) ;
     BEXP := 2100  (*2**11 + 52; P9-characteristic*) ;
     while FNW < D25 do
       begin
         INW := 2 * INW ;
         FNW := 2 * FNW + INW DIV D26 ;
         INW := INW MOD D26 ;
         BEXP := BEXP - 1
       end (* while *) ;
     if ELSC > 0 then
       repeat
         FNW := 5 * FNW ;
         INW := ( FNW MOD 8 ) * D23 + ( 5 * INW ) DIV 8 ;
         FNW := FNW DIV 8 ;
         if FNW < D25 then
           begin
             INW := 2 * INW ;
             FNW := 2 * FNW + INW DIV D26 ;
             INW := INW MOD D26 ;
             BEXP := BEXP - 1
           end (* then *) ;
         BEXP := BEXP + 4 ;
         ELSC := ELSC - 1 ;
       until ELSC = 0
     else
       if ELSC < 0 then
         repeat
           if FNW >= 5 * D23 then
             begin
               INW := INW DIV 2 + ( FNW MOD 2 ) * D25 ;
               FNW := FNW DIV 2 ;
               BEXP := BEXP + 1
             end (* then *) ;
           INW := 8 * INW ;
           FNW := 8 * FNW + INW DIV D26 ;
           INW := INW MOD D26 + FNW MOD 5 * D26 ;
           FNW := FNW DIV 5 ;
           INW := INW DIV 5 ;
           BEXP := BEXP - 4 ;
           ELSC := ELSC + 1
         until ELSC = 0 ;
     INW := INW + 2048 ;
     if INW >= D26 then
       begin
         INW := 0 ;
         FNW := FNW + 1 ;
         if FNW = D26 then
           begin
             FNW := D25 ;
             BEXP := BEXP + 1
           end (* then *)
       end (* then *) ;
     if ( BEXP < 0 ) or ( BEXP > 4095 ) then
       STOP ( 4 ) ;
     INW := ( INW DIV 4096 ) * 4096 + BEXP ;
     DFLAG := 1 ;
     4 :
     OFLAG := 0 ;
     5 :
     
   end (* READ_UNTIL_NEXT_DELI *) ;



procedure FILL_T_LIST ( N : INTEGER ) ;

   begin (* FILL_T_LIST *)
     STORE [ TLSC ] := N ;
     TLSC := TLSC + 1
   end (* FILL_T_LIST *) ;



procedure PRESCAN ;                                                   (*HK*)

   label 1 , 2 , 3 , 4 , 5 , 6 , 7 ;

   var BC , MBC : INTEGER ;


   procedure FILL_PRESCAN_LIST ( N : INTEGER ) ; (*n = 0 or n = 1*)
                                                (*HF            *)

      var I , J , K : INTEGER ;

      begin (* FILL_PRESCAN_LIST *)
        K := PLIB ;
        PLIB := K - DFLAG - 1 ;
        J := K ;
        for I := 2 * BC + N DOWNTO 1 do
          begin
            K := STORE [ J ] ;
            STORE [ J ] := K - DFLAG - 1 ;
            J := K
          end (* for *) ;

        (*************************************************************)
        (*shift lower part of prescan_list down over dfag + 1 places:*)
        (*************************************************************)

        K := PLIB ;
        if DFLAG = 0 then
          for I := J - PLIB DOWNTO 1 do
            begin
              STORE [ K ] := STORE [ K + 1 ] ;
              K := K + 1
            end (* for *)
        else
          begin  (*shift:*)
            for I := J - PLIB - 1 DOWNTO 1 do
              begin
                STORE [ K ] := STORE [ K + 2 ] ;
                K := K + 1
              end (* for *) ;

        (****************************)
        (*enter fnw in prescan_list:*)
        (****************************)

            STORE [ K + 1 ] := FNW
          end (* else *) ;

        (****************************)
        (*enter inw in prescan_list:*)
        (****************************)

        STORE [ K ] := INW
      end (* FILL_PRESCAN_LIST *) ;


   procedure AUGMENT_PRESCAN_LIST ;                                   (*HH*)

      begin (* AUGMENT_PRESCAN_LIST *)
        DFLAG := 1 ;
        INW := PLIE ;
        FNW := PLIE - 1 ;
        FILL_PRESCAN_LIST ( 0 )
      end (* AUGMENT_PRESCAN_LIST *) ;


   procedure BLOCK_INTRODUCTION ;                                     (*HK*)

      begin (* BLOCK_INTRODUCTION *)
        FILL_T_LIST ( BC ) ;
        FILL_T_LIST ( - 1 )  (*block-begin marker*) ;
        MBC := MBC + 1 ;
        BC := MBC ;
        AUGMENT_PRESCAN_LIST
      end (* BLOCK_INTRODUCTION *) ;


   begin (* PRESCAN *)
     PLIB := PLIE ;
     STORE [ PLIE ] := PLIE - 1 ;
     TLSC := TLIB ;
     BC := 0 ;
     MBC := 0 ;
     QC := 0 ;
     RHT := 0 ;
     VHT := 0 ;
     FILL_T_LIST ( DL ) ; (*dl should be 'begin'*)
     AUGMENT_PRESCAN_LIST ;
     1 :
     BFLAG := 0 ;
     2 :
     READ_UNTIL_NEXT_DELImiter ;
     3 :
     if DL <= 84

     (************************************************************)
     (*+,-,*,/,_:,|^,>,>=,=,<=,<,|=,~,^,',_~,_=,goto,if,then,else*)
     (************************************************************)

     then  (*skip:*)
       goto 1 ;
     if DL = 85 (*for*)
     then
       begin
         BLOCK_INTRODUCTION ;
         goto 1
       end (* then *) ;
     if DL <= 89  (*do,comma,period,ten*) then  (*skip:*)
       goto 1 ;
     if DL = 90  (*:*) then
       begin
         FILL_PRESCAN_LIST ( 0 ) ;
         goto 2
       end (* then *) ;
     if DL = 91 (*;*)
     then
       begin
         while STORE [ TLSC - 1 ] < 0  (*block-begin marker*) do
           begin
             TLSC := TLSC - 2 ;
             BC := STORE [ TLSC ]
           end (* while *) ;
         if RHT <> 0 then
           STOP ( 22 ) ;
         if VHT <> 0 then
           STOP ( 23 ) ;
         goto 1
       end (* then *) ;
     if DL <= 97  (*:=,step,until,while,comment*) then  (*skip:*)
       goto 1 ;
     if DL <= 99 (*(,)*)
     then
       begin
         if DL = 98 then
           RHT := RHT + 1
         else
           RHT := RHT - 1 ;
         goto 1
       end (* then *) ;
     if DL <= 101 (*[,]*)
     then
       begin
         if DL = 100 then
           VHT := VHT + 1
         else
           VHT := VHT - 1 ;
         goto 1
       end (* then *) ;
     if DL = 102 (*|<*)
     then
       begin
         repeat
           if DL = 102  (*|<*) then
             QC := QC + 1 ;
           if DL = 103  (*|>*) then
             QC := QC - 1 ;
           if QC > 0 then
             READ_NEXT_SYMBOL
         until QC = 0 ;
         goto 2
       end (* then *) ;
     if DL = 104 (*begin*)
     then
       begin
         FILL_T_LIST ( DL ) ;
         if BFLAG <> 0 then
           goto 1 ;
         READ_UNTIL_NEXT_DELImiter ;
         if ( DL <= 105 ) or ( DL > 112 ) then
           goto 3 ;
         TLSC := TLSC - 1  (*remove begin from t_list*) ;
         BLOCK_INTRODUCTION ;
         FILL_T_LIST ( 104 )  (*add begin to t_list again*) ;
         goto 3 ;
       end (* then *) ;
     if DL = 105 (*end*)
     then
       begin
         while STORE [ TLSC - 1 ] < 0  (*block-begin marker*) do
           begin
             TLSC := TLSC - 2 ;
             BC := STORE [ TLSC ]
           end (* while *) ;
         if RHT <> 0 then
           STOP ( 22 ) ;
         if VHT <> 0 then
           STOP ( 23 ) ;
         TLSC := TLSC - 1  (*remove corresponding begin from t_list*) ;
         if TLSC > TLIB then
           goto 1 ;
         goto 7   (*end of prescan*)
       end (* then *) ;
     if DL <= 105  (*dl = |>*) then
       goto 1 ;
     if DL = 111 (*switch*)
     then
       if BFLAG = 0 then (*declarator*)
         begin
           READ_UNTIL_NEXT_DELImiter  (*for switch identifier*) ;
           FILL_PRESCAN_LIST ( 0 ) ;
           goto 6
         end (* then *)
       else   (*specifier*)
         goto 5 ;
     4 :
     if DL = 112 (*procedure*)
     then
       if BFLAG = 0 then (*declarator*)
         begin
           BFLAG := 1 ;
           READ_UNTIL_NEXT_DELImiter  (*for procedure identifier*) ;
           FILL_PRESCAN_LIST ( 1 ) ;
           BLOCK_INTRODUCTION ;
           goto 6
         end (* then *)
       else    (*specificier*)
         goto 5 ;
     if DL > 117  (*false*) then
       STOP ( 8 ) ;
     5 :
     READ_UNTIL_NEXT_DELImiter ;
     6 :
     if DL <> 91  (*;*) then
       goto 4 ;
     goto 2 ;
     7 :
     
   end (* PRESCAN *) ;



procedure INTRO_NEW_BLOCK2 ;                                          (*HW*)

   label 1 ;

   var I , W : INTEGER ;

   begin (* INTRO_NEW_BLOCK2 *)
     INBA := D17 + D15 ;
     1 :
     I := PLIB ;
     PLIB := STORE [ I ] ;
     I := I + 1 ;
     while I <> PLIB do
       begin
         W := STORE [ I ] ;
         if W MOD 8 = 0 (*at most 4 letters/digits*)
         then
           I := I + 1
         else
           begin
             STORE [ NLIB + NLSC ] := STORE [ I + 1 ] ;
             I := I + 2 ;
             NLSC := NLSC + 1
           end (* else *) ;
         STORE [ NLIB + NLSC ] := W ;
         NLSC := NLSC + 2 ;
         if NLIB + NLSC > I then
           STOP ( 15 ) ;
         STORE [ NLIB + NLSC - 1 ] := BN * D19 + INBA
       end (* while *) ;
     if INBA <> D18 + D15 then
       begin
         INBA := D18 + D15 ;
         goto 1
       end (* then *) ;
     LVC := 0
   end (* INTRO_NEW_BLOCK2 *) ;



procedure INTRO_NEW_BLOCK1 ;                                          (*HW*)

   begin (* INTRO_NEW_BLOCK1 *)
     FILL_T_LIST ( NLSC ) ;
     FILL_T_LIST ( 161 ) ;
     INTRO_NEW_BLOCK2
   end (* INTRO_NEW_BLOCK1 *) ;



procedure INTRO_NEW_BLOCK ;                                           (*HW*)

   begin (* INTRO_NEW_BLOCK *)
     BN := BN + 1 ;
     INTRO_NEW_BLOCK1
   end (* INTRO_NEW_BLOCK *) ;



procedure BIT_STRING_MAKER ( W : INTEGER ) ;                          (*LL*)

   var HEAD , TAIL , I : INTEGER ;

   begin (* BIT_STRING_MAKER *)
     HEAD := 0 ;
     TAIL := W MOD D10 ;

     (************************************************)
     (*shift (head,tail) bitcount places to the left:*)
     (************************************************)

     for I := 1 to BITCOUNT do
       begin
         HEAD := 2 * HEAD + TAIL DIV D26 ;
         TAIL := ( TAIL MOD D26 ) * 2
       end (* for *) ;
     BITSTOCK := BITSTOCK + TAIL ;
     BITCOUNT := BITCOUNT + W DIV D10 ;
     if BITCOUNT > 27 then
       begin
         BITCOUNT := BITCOUNT - 27 ;
         STORE [ RNSB ] := BITSTOCK ;
         BITSTOCK := HEAD ;
         RNSB := RNSB + 1 ;
         if RNSB = RNSD then
           if NLIB + NLSC + 8 < PLIB then
             begin         (*shift text, fli, kli and nli*)
               for I := NLIB + NLSC - RNSD - 1 DOWNTO 0 do
                 STORE [ RNSD + I + 8 ] := STORE [ RNSD + I ] ;
               RNSD := RNSD + 8 ;
               FLIB := FLIB + 8 ;
               KLIB := KLIB + 8 ;
               NLIB := NLIB + 8
             end (* then *)
           else
             STOP ( 25 )
       end (* then *)
   end (* BIT_STRING_MAKER *) ;



procedure ADDRESS_CODER ( A : INTEGER ) ;                             (*LS*)

   var W : INTEGER ;

   begin (* ADDRESS_CODER *)
     W := A MOD D5 ;
     if W = 1 then
       W := 2048  (*2*1024 +  0*)
     else
       if W = 2 then
         W := 3074  (*3*1024 +  2*)
       else
         if W = 3 then
           W := 3075       (*3*1024 +  3*)
         else
           W := 6176  (*6*1024 + 32*) + W ;
     BIT_STRING_MAKER ( W ) ;
     W := ( A DIV D5 ) MOD D5 ;
     if W = 0 then
       W := 2048  (*2*1024 +  0*)
     else
       if W = 1 then
         W := 4100  (*4*1024 +  4*)
       else
         if W = 2 then
           W := 4101  (*4*1024 +  5*)
         else
           if W = 4 then
             W := 4102  (*4*1024 +  6*)
           else
             if W = 5 then
               W := 4103   (*4*1024 +  7*)
             else
               W := 6176  (*6*1024 + 32*) + W ;
     BIT_STRING_MAKER ( W ) ;
     W := ( A DIV D10 ) MOD D5 ;
     if W = 0 then
       W := 1024           (*1*1024 + 0*)
     else
       W := 6176  (*6*1024 + 32*) + W ;
     BIT_STRING_MAKER ( W )
   end (* ADDRESS_CODER *) ;



procedure FILL_RESULT_LIST ( OPC , W : INTEGER ) ;                    (*ZF*)

   var J : 8 .. 61 ;

   begin (* FILL_RESULT_LIST *)
     RLSC := RLSC + 1 ;
     if OPC < 8 then
       begin
         ADDRESS_CODER ( W ) ;
         W := ( W DIV D15 ) * D15 + OPC ;
         if W = 21495808  (*  2S   0 A  *) then
           W := 3076  (*3*1024 +   4*)
         else
           if W = 71827459  (*  2B   3 A  *) then
             W := 3077  (*3*1024 +   5*)
           else
             if W = 88080386  (*  2T 2X0    *) then
               W := 4108  (*4*1024 +  12*)
             else
               if W = 71827456  (*  2B   0 A  *) then
                 W := 4109  (*4*1024 +  13*)
               else
                 if W = 4718592  (*  2A   0 A  *) then
                   W := 7280  (*7*1024 + 112*)
                 else
                   if W = 71303170  (*  2B 2X0    *) then
                     W := 7281  (*7*1024 + 113*)
                   else
                     if W = 88604673  (*  2T   1 A  *) then
                       W := 7282  (*7*1024 + 114*)
                     else
                       if W = 0  (*  0A 0X0    *) then
                         W := 7283  (*7*1024 + 115*)
                       else
                         if W = 524291  (*  0A   3 A  *) then
                           W := 7284  (*7*1024 + 116*)
                         else
                           if W = 88178690  (*N 2T 2X0    *) then
                             W := 7285  (*7*1024 + 117*)
                           else
                             if W = 71827457  (*  2B   1 A  *) then
                               W := 7286  (*7*1024 + 118*)
                             else
                               if W = 1048577  (*  0A 1X0 B  *) then
                                 W := 7287  (*7*1024 + 119*)
                               else
                                 if W = 20971522  (*  2S 2X0    *) then
                                   W := 7288  (*7*1024 + 120*)
                                 else
                                   if W = 4784128  (*Y 2A   0 A  *)
                                   then
                                     W := 7289  (*7*1024 + 121*)
                                   else
                                     if W = 8388608  (*  4A 0X0    *)
                                     then
                                       W := 7290  (*7*1024 + 122*)
                                     else
                                       if W = 4390912 
                                                   (*Y 2A 0X0   P*)then
                                         W := 7291  (*7*1024 + 123*)
                                       else
                                         if W = 13172736

     (**************)
     (*Y 6A   0 A  *)
     (**************)

                                         then
                                           W := 7292  (*7*1024 + 124*)
                                         else
                                           if W = 1572865

     (**************)
     (*  0A 1X0 C  *)
     (**************)

                                           then
                                             W := 7293 
                                                   (*7*1024 + 125*)
                                           else
                                             if W = 524288

     (**************)
     (*  0A   0 A  *)
     (**************)

                                             then
                                               W := 7294

     (**************)
     (*7*1024 + 126*)
     (**************)

                                             else
                                               begin
                                                 ADDRESS_CODER ( W DIV
                                                   D15 + OPC * D12 ) ;
                                                 W := 7295

     (**************)
     (*7*1024 + 127*)
     (**************)

                                               end (* else *)
       end (* then *)
     else
       if OPC <= 61 then
         begin
           J := OPC ;
           case J of
             8 : W := 10624  (*10*1024+384*) ;
             9 : W := 6160  (* 6*1024+ 16*) ;
             10 : W := 10625  (*10*1024+385*) ;
             11 : W := 10626  (*10*1024+386*) ;
             12 : W := 10627  (*10*1024+387*) ;
             13 : W := 7208  (* 7*1024+ 40*) ;
             14 : W := 6161  (* 6*1024+ 17*) ;
             15 : W := 10628  (*10*1024+388*) ;
             16 : W := 5124  (* 5*1024+  4*) ;
             17 : W := 7209  (* 7*1024+ 41*) ;
             18 : W := 6162  (* 6*1024+ 18*) ;
             19 : W := 7210  (* 7*1024+ 42*) ;
             20 : W := 7211  (* 7*1024+ 43*) ;
             21 : W := 10629  (*10*1024+389*) ;
             22 : W := 10630  (*10*1024+390*) ;
             23 : W := 10631  (*10*1024+391*) ;
             24 : W := 10632  (*10*1024+392*) ;
             25 : W := 10633  (*10*1024+393*) ;
             26 : W := 10634  (*10*1024+394*) ;
             27 : W := 10635  (*10*1024+395*) ;
             28 : W := 10636  (*10*1024+396*) ;
             29 : W := 10637  (*10*1024+397*) ;
             30 : W := 6163  (* 6*1024+ 19*) ;
             31 : W := 7212  (* 7*1024+ 44*) ;
             32 : W := 10638  (*10*1024+398*) ;
             33 : W := 4096  (* 4*1024+  0*) ;
             34 : W := 4097  (* 4*1024+  1*) ;
             35 : W := 7213  (* 7*1024+ 45*) ;
             36 : W := 10639  (*10*1024+399*) ;
             37 : W := 10640  (*10*1024+400*) ;
             38 : W := 10641  (*10*1024+401*) ;
             39 : W := 7214  (* 7*1024+ 46*) ;
             40 : W := 10642  (*10*1024+402*) ;
             41 : W := 10643  (*10*1024+403*) ;
             42 : W := 10644  (*10*1024+404*) ;
             43 : W := 10645  (*10*1024+405*) ;
             44 : W := 10646  (*10*1024+406*) ;
             45 : W := 10647  (*10*1024+407*) ;
             46 : W := 10648  (*10*1024+408*) ;
             47 : W := 10649  (*10*1024+409*) ;
             48 : W := 10650  (*10*1024+410*) ;
             49 : W := 10651  (*10*1024+411*) ;
             50 : W := 10652  (*10*1024+412*) ;
             51 : W := 10653  (*10*1024+413*) ;
             52 : W := 10654  (*10*1024+414*) ;
             53 : W := 10655  (*10*1024+415*) ;
             54 : W := 10656  (*10*1024+416*) ;
             55 : W := 10657  (*10*1024+417*) ;
             56 : W := 5125  (* 5*1024+  5*) ;
             57 : W := 10658  (*10*1024+418*) ;
             58 : W := 5126  (* 5*1024+  6*) ;
             59 : W := 10659  (*10*1024+419*) ;
             60 : W := 10660  (*10*1024+420*) ;
             61 : W := 7215                             (* 7*1024+ 47*)
           end (* case *)
         end (* then *)
       else
         if OPC = 85 (*ST*)
         then
           W := 5127 (* 5*1024 +   7*)
         else
           W := 10599  (*10*1024 + 359*) + OPC ;
     BIT_STRING_MAKER ( W )
   end (* FILL_RESULT_LIST *) ;



procedure MAIN_SCAN ;                                                 (*EL*)

   label 1 , 2 , 3 , 64 , 66 , 69 , 70 , 76 , 81 , 82 , 8201 , 8202 ,
         83 , 8301 , 84 , 8401 , 85 , 8501 , 86 , 8601 , 87 , 8701 ,
         8702 , 8703 , 8704 , 8705 , 90 , 91 , 92 , 94 , 95 , 96 , 98 ,
         9801 , 9802 , 9803 , 9804 , 99 , 100 , 101 , 102 , 104 , 105 ,
         1052 , 106 , 107 , 108 , 1081 , 1082 , 1083 , 109 , 110 , 1101
         , 1102 , 1103 , 111 , 112 , 1121 , 1122 , 1123 , 1124 ;


   procedure FILL_T_LIST_WITH_DELimiter ;                             (*ZW*)

      begin (* FILL_T_LIST_WITH_DEL *)
        FILL_T_LIST ( D8 * OH + DL )
      end (* FILL_T_LIST_WITH_DEL *) ;


   procedure FILL_FUTURE_LIST ( PLACE , VALUE : INTEGER ) ;           (*FU*)

      var I : INTEGER ;

      begin (* FILL_FUTURE_LIST *)
        if PLACE >= KLIB then
          begin
            if NLIB + NLSC + 16 >= PLIB then
              STOP ( 6 ) ;
            for I := NLIB + NLSC - 1 DOWNTO KLIB do
              STORE [ I + 16 ] := STORE [ I ] ;
            KLIB := KLIB + 16 ;
            NLIB := NLIB + 16
          end (* then *) ;
        STORE [ PLACE ] := VALUE
      end (* FILL_FUTURE_LIST *) ;


   procedure FILL_CONSTANT_LIST ( N : INTEGER ) ;                     (*KU*)

      var I : INTEGER ;

      begin (* FILL_CONSTANT_LIST *)
        if KLIB + KLSC = NLIB then
          begin
            if NLIB + NLSC + 16 >= PLIB then
              STOP ( 18 ) ;
            for I := NLIB + NLSC - 1 DOWNTO NLIB do
              STORE [ I + 16 ] := STORE [ I ] ;
            NLIB := NLIB + 16
          end (* then *) ;
        if N >= 0 then
          STORE [ KLIB + KLSC ] := N
        else  (*one's complement representation*)
          STORE [ KLIB + KLSC ] := MZ + N ;
        KLSC := KLSC + 1
      end (* FILL_CONSTANT_LIST *) ;


   procedure UNLOAD_T_LIST_ELEMENt ( var VARIABLE : INTEGER ) ;       (*ZU*)

      begin (* UNLOAD_T_LIST_ELEMEN *)
        TLSC := TLSC - 1 ;
        VARIABLE := STORE [ TLSC ]
      end (* UNLOAD_T_LIST_ELEMEN *) ;


   procedure FILL_OUTPUT ( C : INTEGER ) ;

      begin (* FILL_OUTPUT *)
        POS := POS + 1 ;
        if C < 10 then
          WRITE ( CHR ( C + ORD ( '0' ) ) )
        else
          if C < 36 then
            WRITE ( CHR ( C - 10 + ORD ( 'a' ) ) )
          else
            if C < 64 then
              WRITE ( CHR ( C - 37 + ORD ( 'A' ) ) )
            else
              if C = 184 then
                WRITE ( ' ' )
              else
                if C = 138 then
                  begin
                    WRITE ( ' ' : 8 - ( POS - 1 ) MOD 8 ) ;
                    POS := POS + 8 - ( POS - 1 ) MOD 8
                  end (* then *)
                else
                  begin
                    WRITELN ;
                    POS := 0
                  end (* else *)
      end (* FILL_OUTPUT *) ;


   procedure OFFER_CHARACTER_TO_Typewriter ( C : INTEGER ) ;          (*HS*)

      begin (* OFFER_CHARACTER_TO_T *)
        C := C MOD 64 ;
        if C < 63 then
          FILL_OUTPUT ( C )
      end (* OFFER_CHARACTER_TO_T *) ;


   procedure LABEL_DECLARATION ;                                      (*FY*)

      var ID , ID2 , I , W : INTEGER ;

      begin (* LABEL_DECLARATION *)
        ID := STORE [ NLIB + NID ] ;
        if ( ID DIV D15 ) MOD 2 = 0 then
          begin  (*preceding applied occurrences*)
            FILL_FUTURE_LIST ( FLIB + ID MOD D15 , RLSC )
          end (* then *)
        else (*first occurrence*)
          STORE [ NLIB + NID ] := ID - D15 + 1 * D24 + RLSC ;
        ID := STORE [ NLIB + NID - 1 ] ;
        if ID MOD D3 = 0 then
          begin  (*at most 4 letters/digits*)
            I := 4 ;
            ID := ID DIV D3 ;
            while ( ID MOD D6 ) = 0  (*void*) do
              begin
                I := I - 1 ;
                ID := ID DIV D6
              end (* while *) ;
            repeat
              OFFER_CHARACTER_TO_Typewriter ( ID ) ;
              I := I - 1 ;
              ID := ID DIV D6
            until I = 0
          end (* then *)
        else
          begin
            ID2 := STORE [ NLIB + NID - 2 ] ;
            ID2 := ID2 DIV D3 + ( ID2 MOD D3 ) * D24 ;
            W := ( ID2 MOD D24 ) * D3 + ID DIV D24 ;
            ID := ( ID MOD D24 ) * D3 + ID2 DIV D24 ;
            ID2 := W ;
            I := 9 ;
            repeat
              OFFER_CHARACTER_TO_Typewriter ( ID ) ;
              I := I - 1 ;
              W := ID2 DIV D6 + ( ID MOD D6 ) * D21 ;
              ID := ID DIV D6 + ( ID2 MOD D6 ) * D21 ;
              ID2 := W
            until I = 0
          end (* else *) ;
        FILL_OUTPUT ( 138  (*TAB*) ) ;
        W := RLSC ;
        for I := 1 to 3 do
          begin
            OFFER_CHARACTER_TO_Typewriter ( W DIV D10 DIV 10 ) ;
            OFFER_CHARACTER_TO_Typewriter ( W DIV D10 MOD 10 ) ;
            W := ( W MOD D10 ) * D5 ;
            if I < 3 then
              FILL_OUTPUT ( 184  (*SPACE*) )
          end (* for *) ;
        FILL_OUTPUT ( 139  (*NLCR*) )
      end (* LABEL_DECLARATION *) ;


   procedure TEST_FIRST_OCCURRENCe ;                                  (*LF*)

      begin (* TEST_FIRST_OCCURRENC *)
        ID := STORE [ NLIB + NID ] ;
        if ( ID DIV D15 ) MOD 2 = 1 (*first occurrence*)
        then
          begin
            ID := ID - D15 - ID MOD D15 + 2 * D24 + FLSC ;
            if NID <= NLSC0  (*MCP*)
            then
              FILL_FUTURE_LIST ( FLIB + FLSC , STORE [ NLIB + NID ] ) ;
            STORE [ NLIB + NID ] := ID ;
            FLSC := FLSC + 1
          end (* then *)
      end (* TEST_FIRST_OCCURRENC *) ;


   procedure NEW_BLOCK_BY_DECL1 ;                              (*HU*)

      begin (* NEW_BLOCK_BY_DECL1 *)
        FILL_RESULT_LIST ( 0 , 71827456 + BN )  (*2B 'bn' A*) ;
        FILL_RESULT_LIST ( 89  (*SCC*) , 0 ) ;
        PNLV := 5 * 32 + BN ;
        VLAM := PNLV
      end (* NEW_BLOCK_BY_DECL1 *) ;


   procedure NEW_BLOCK_BY_DECL ;                               (*HU*)

      begin (* NEW_BLOCK_BY_DECL *)
        if STORE [ TLSC - 2 ] <> 161                           (*block-begin marker*)
        then
          begin
            TLSC := TLSC - 1  (*remove 'begin'*) ;
            FILL_RESULT_LIST ( 0 , 4718592 )  (*2A 0 A*) ;
            FILL_RESULT_LIST ( 1 , 71827456 + RLSC + 3 )

        (***************)
        (*2B 'rlsc+3' A*)
        (***************)

                               ;
            FILL_RESULT_LIST ( 9  (*ETMP*) , 0 ) ;
            FILL_RESULT_LIST ( 2 , 88080384 + FLSC )  (*2T 'flsc'*) ;
            FILL_T_LIST ( FLSC ) ;
            FLSC := FLSC + 1 ;
            INTRO_NEW_BLOCK ;
            FILL_T_LIST ( 104  (*begin*) ) ;
            NEW_BLOCK_BY_DECL1
          end (* then *)
      end (* NEW_BLOCK_BY_DECL *) ;


   procedure FILL_NAME_LIST ;                                         (*HN*)

      begin (* FILL_NAME_LIST *)
        NLSC := NLSC + DFLAG + 2 ;
        if NLSC + NLIB > PLIB then
          STOP ( 16 ) ;
        STORE [ NLIB + NLSC - 1 ] := ID ;
        STORE [ NLIB + NLSC - 2 ] := INW ;
        if INW MOD D3 > 0 then
          STORE [ NLIB + NLSC - 3 ] := FNW
      end (* FILL_NAME_LIST *) ;


   procedure RESERVATION_OF_LOCAL_variables ;                         (*KY*)

      begin (* RESERVATION_OF_LOCAL *)
        if LVC > 0 then
          begin
            FILL_RESULT_LIST ( 0 , 4718592 + LVC )  (*2A 'lvc' A*) ;
            FILL_RESULT_LIST ( 0 , 8388657 )  (*4A 17X1*) ;
            FILL_RESULT_LIST ( 0 , 8388658 ) (*4A 18X1*)
          end (* then *)
      end (* RESERVATION_OF_LOCAL *) ;


   procedure ADDRESS_TO_REGISTER ;                                    (*ZR*)

      begin (* ADDRESS_TO_REGISTER *)
        if ID DIV D15 MOD 2 = 0                                       (*static addressing*)
        then
          if ID DIV D24 MOD D2 = 2                                    (*future list*)
          then
            FILL_RESULT_LIST ( 2 , 71303168 + ID MOD D15

        (******************)
        (*2B 'FLI-address'*)
        (******************)

                               )
          else
            FILL_RESULT_LIST ( ID DIV D24 MOD 4 , 71827456 + ID MOD D15

        (***********************)
        (*2B 'static address' A*)
        (***********************)

                               )
        else
          FILL_RESULT_LIST ( 0 , 21495808 + ID MOD D15

        (************************)
        (*2S 'dynamic address' A*)
        (************************)

                             )
      end (* ADDRESS_TO_REGISTER *) ;


   procedure GENERATE_ADDRESS ;                                       (*ZH*)

      var OPC : INTEGER ;

      begin (* GENERATE_ADDRESS *)
        ADDRESS_TO_REGISTER ;
        if ( ID DIV D16 ) MOD 2 = 1 then  (*formal*)
          FILL_RESULT_LIST ( 18  (*TFA*) , 0 )
        else
          begin
            OPC := 14  (*TRAD*) ;
            if ( ID DIV D15 ) MOD 2 = 0 then
              OPC := OPC + 1  (*TRAS*) ;
            if ( ID DIV D19 ) MOD 2 = 1 then
              OPC := OPC + 2  (*TIAD or TIAS*) ;
            FILL_RESULT_LIST ( OPC , 0 )
          end (* else *)
      end (* GENERATE_ADDRESS *) ;


   procedure RESERVATION_OF_ARRAYs ;                                  (*KN*)

      begin (* RESERVATION_OF_ARRAY *)
        if VLAM <> 0 then
          begin
            VLAM := 0 ;
            if STORE [ TLSC - 1 ] = 161 (*block-begin marker*)
            then
              RLAA := NLIB + STORE [ TLSC - 2 ]
            else
              RLAA := NLIB + STORE [ TLSC - 3 ] ;
            RLAB := NLIB + NLSC ;
            while RLAB <> RLAA do
              begin
                ID := STORE [ RLAB - 1 ] ;
                if ( ID >= D26 ) and ( ID < D25 + D26 ) then
                  begin   (*value array:*)
                    ADDRESS_TO_REGISTER ;
                    if ( ID DIV D19 ) MOD 2 = 0 then
                      FILL_RESULT_LIST ( 92  (*RVA*) , 0 )
                    else
                      FILL_RESULT_LIST ( 93  (*IVA*) , 0 ) ;
                    STORE [ RLAB - 1 ] := ( ID DIV D15 ) * D15 - D16 +
                                          PNLV ;
                    PNLV := PNLV + 8 * 32  (*at most 5 indices*)
                  end (* then *) ;
                if STORE [ RLAB - 2 ] MOD D3 = 0 then
                  RLAB := RLAB - 2
                else
                  RLAB := RLAB - 3
              end (* while *) ;
            RLAB := NLIB + NLSC ;
            while RLAB <> RLAA do
              begin
                if STORE [ RLAB - 1 ] >= D26 then
                  begin
                    ID := STORE [ RLAB - 1 ] - D26 ;
                    if ID < D25 then
                      begin
                        ADDRESS_TO_REGISTER ;
                        FILL_RESULT_LIST ( 95  (*VAP*) , 0 )
                      end (* then *)
                    else
                      begin
                        ID := ID - D25 ;
                        ADDRESS_TO_REGISTER ;
                        FILL_RESULT_LIST ( 94  (*LAP*) , 0 )
                      end (* else *)
                  end (* then *) ;
                if STORE [ RLAB - 2 ] MOD D3 = 0 then
                  RLAB := RLAB - 2
                else
                  RLAB := RLAB - 3
              end (* while *) ;
            if NFLAG <> 0 then
              ID := STORE [ NLIB + NID ]
          end (* then *)
      end (* RESERVATION_OF_ARRAY *) ;


   procedure PROCEDURE_STATEMENT ;                                    (*LH*)

      begin (* PROCEDURE_STATEMENT *)
        if EFLAG = 0 then
          RESERVATION_OF_ARRAYs ;
        if NID > NLSCOP then
          begin
            if FFLAG = 0 then
              TEST_FIRST_OCCURRENCe ;
            ADDRESS_TO_REGISTER
          end (* then *)
        else
          begin
            FILL_T_LIST ( STORE [ NLIB + NID ] MOD D12 ) ;
            if DL = 98 (*(*)
            then
              begin
                EFLAG := 1 ;
                goto 9801
              end (* then *)
          end (* else *)
      end (* PROCEDURE_STATEMENT *) ;


   procedure PRODUCTION_TRANSMARK ;                                   (*ZL*)

      begin (* PRODUCTION_TRANSMARK *)
        FILL_RESULT_LIST ( 9 + 2 * FFLAG - EFLAG , 0 )
      end (* PRODUCTION_TRANSMARK *) ;


   procedure PRODUCTION_OF_OBJECT_program ( OPHT : INTEGER ) ;        (*ZS*)

      var OPERATOR , BLOCK_NUMBER : INTEGER ;

      begin (* PRODUCTION_OF_OBJECT *)
        OH := OPHT ;
        if NFLAG <> 0 then
          begin
            NFLAG := 0 ;
            AFLAG := 0 ;
            if PFLAG = 0 then
              if JFLAG = 0 then
                begin
                  ADDRESS_TO_REGISTER ;
                  if OH > ( STORE [ TLSC - 1 ] DIV D8 ) MOD 16 then
                    OPERATOR := 315         (*5*63*)
                  else
                    begin
                      OPERATOR := STORE [ TLSC - 1 ] MOD D8 ;
                      if ( OPERATOR <= 63 ) or ( OPERATOR > 67 ) then
                        OPERATOR := 315            (*5*63*)
                      else
                        begin
                          TLSC := TLSC - 1 ;
                          OPERATOR := 5 * OPERATOR
                        end (* else *)
                    end (* else *) ;
                  if FFLAG = 0 then
                    begin
                      if ID DIV D15 MOD 2 = 0 then
                        OPERATOR := OPERATOR + 1 ;
                      if ID DIV D19 MOD 2 <> 0 then
                        OPERATOR := OPERATOR + 2 ;
                      FILL_RESULT_LIST ( OPERATOR - 284 , 0 )
                    end (* then *)
                  else
                    FILL_RESULT_LIST ( OPERATOR - 280 , 0 )
                end (* then *)
              else
                if FFLAG = 0 then
                  begin
                    BLOCK_NUMBER := ID DIV D19 MOD D5 ;
                    if BLOCK_NUMBER <> BN then
                      begin
                        FILL_RESULT_LIST ( 0 , 71827456 + BLOCK_NUMBER
                                           ) ;
                        FILL_RESULT_LIST ( 28  (*GTA*) , 0 )
                      end (* then *) ;
                    TEST_FIRST_OCCURRENCe ;
                    if ID DIV D24 MOD 4 = 2 then
                      FILL_RESULT_LIST ( 2 , 88080384 + ID MOD D15 )

        (**************)
        (*2T 'address'*)
        (**************)

                    else
                      FILL_RESULT_LIST ( 1 , 88604672 + ID MOD D15 )

        (****************)
        (*2T 'address' A*)
        (****************)

                  end (* then *)
                else
                  begin
                    ADDRESS_TO_REGISTER ;
                    FILL_RESULT_LIST ( 35  (*TFR*) , 0 )
                  end (* else *)
            else
              begin
                PROCEDURE_STATEMENT ;
                if NID > NLSCOP then
                  begin
                    FILL_RESULT_LIST ( 0 , 4718592  (*2A 0 A*) ) ;
                    PRODUCTION_TRANSMARK
                  end (* then *)
              end (* else *)
          end (* then *)
        else
          if AFLAG <> 0 then
            begin
              AFLAG := 0 ;
              FILL_RESULT_LIST ( 58  (*TAR*) , 0 )
            end (* then *) ;
        while OH <= STORE [ TLSC - 1 ] DIV D8 MOD 16 do
          begin
            TLSC := TLSC - 1 ;
            OPERATOR := STORE [ TLSC ] MOD D8 ;
            if ( OPERATOR > 63 ) and ( OPERATOR <= 80 ) then
              FILL_RESULT_LIST ( OPERATOR - 5 , 0 )
            else
              if OPERATOR = 132 (*NEG*)
              then
                FILL_RESULT_LIST ( 57  (*NEG*) , 0 )
              else
                if ( OPERATOR < 132 ) and ( OPERATOR > 127 ) then
                  begin (*ST,STA,STP,STAP*)
                    if OPERATOR > 129 then
                      begin (*STP,STAP*)
                        TLSC := TLSC - 1 ;
                        FILL_RESULT_LIST ( 0 , 71827456 + STORE [ TLSC
                                           ]  (*2B 'BN' A*) )
                      end (* then *) ;
                    FILL_RESULT_LIST ( OPERATOR - 43 , 0 )
                  end (* then *)
                else (*special function*)
                  if ( OPERATOR > 127 ) and ( OPERATOR <= 141 ) then
                    FILL_RESULT_LIST ( OPERATOR - 57 , 0 )
                  else
                    if ( OPERATOR > 141 ) and ( OPERATOR <= 151 ) then
                      FILL_RESULT_LIST ( OPERATOR - 40 , 0 )
                    else
                      STOP ( 22 )
          end (* while *)
      end (* PRODUCTION_OF_OBJECT *) ;


   function THENELSE : BOOLEAN ;                                      (*ZN*)

      begin (* THENELSE *)
        if ( STORE [ TLSC - 1 ] MOD 255 = 83  (*then*) ) or ( STORE [
        TLSC - 1 ] MOD 255 = 84  (*else*) ) then
          begin
            TLSC := TLSC - 2 ;
            FILL_FUTURE_LIST ( FLIB + STORE [ TLSC ] , RLSC ) ;
            UNLOAD_T_LIST_ELEMENt ( EFLAG ) ;
            THENELSE := TRUE
          end (* then *)
        else
          THENELSE := FALSE
      end (* THENELSE *) ;


   procedure EMPTY_T_LIST_THROUGH_thenelse ;                          (*FR*)

      begin (* EMPTY_T_LIST_THROUGH *)
        OFLAG := 1 ;
        repeat
          PRODUCTION_OF_OBJECT_program ( 1 )
        until not THENELSE
      end (* EMPTY_T_LIST_THROUGH *) ;


   function DO_IN_T_LIST : BOOLEAN ;                                  (*ER*)

      begin (* DO_IN_T_LIST *)
        if STORE [ TLSC - 1 ] MOD 255 = 86 then
          begin
            TLSC := TLSC - 5 ;
            NLSC := STORE [ TLSC + 2 ] ;
            BN := BN - 1 ;
            FILL_FUTURE_LIST ( FLIB + STORE [ TLSC + 1 ] , RLSC + 1 ) ;
            FILL_RESULT_LIST ( 1 , 88604672  (*2T 0X0 A*) + STORE [
                               TLSC ] ) ;
            DO_IN_T_LIST := TRUE
          end (* then *)
        else
          DO_IN_T_LIST := FALSE
      end (* DO_IN_T_LIST *) ;


   procedure LOOK_FOR_NAME ;                                          (*HZ*)

      label 1 , 2 ;

      var I , W : INTEGER ;

      begin (* LOOK_FOR_NAME *)
        I := NLIB + NLSC ;
        1 :
        W := STORE [ I - 2 ] ;
        if W = INW then
          if W MOD 8 = 0 then  (*at most 4 letters/digits*)
            goto 2
          else  (*more than 4 letters/digits*)
            if STORE [ I - 3 ] = FNW then
              goto 2 ;
        if W MOD 8 = 0 then
          I := I - 2
        else
          I := I - 3 ;
        if I > NLIB then
          goto 1 ;
        STOP ( 7 ) ;
        2 :
        NID := I - NLIB - 1 ;
        ID := STORE [ I - 1 ] ;
        PFLAG := ID DIV D18 MOD 2 ;
        JFLAG := ID DIV D17 MOD 2 ;
        FFLAG := ID DIV D16 MOD 2
      end (* LOOK_FOR_NAME *) ;


   procedure LOOK_FOR_CONSTANT ;                                      (*FW*)

      var I : INTEGER ;

      begin (* LOOK_FOR_CONSTANT *)
        if KLIB + KLSC + DFLAG >= NLIB then
          begin  (*move name list*)
            if NLIB + NLSC + 16 >= PLIB then
              STOP ( 5 ) ;
            for I := NLSC - 1 DOWNTO 0 do
              STORE [ NLIB + I + 16 ] := STORE [ NLIB + I ] ;
            NLIB := NLIB + 16
          end (* then *) ;
        if DFLAG = 0 then
          begin  (*search integer constant*)
            STORE [ KLIB + KLSC ] := INW ;
            I := 0 ;
            while STORE [ KLIB + I ] <> INW do
              I := I + 1
          end (* then *)
        else
          begin  (*search floating constant*)
            STORE [ KLIB + KLSC ] := FNW ;
            STORE [ KLIB + KLSC + 1 ] := INW ;
            I := 0 ;
            while ( STORE [ KLIB + I ] <> FNW ) or ( STORE [ KLIB + I +
            1 ] <> INW ) do
              I := I + 1
          end (* else *) ;
        if I = KLSC then  (*first occurrence*)
          KLSC := KLSC + DFLAG + 1 ;
        ID := 3 * D24 + I ;
        if DFLAG = 0 then
          ID := ID + D19 ;
        JFLAG := 0 ;
        PFLAG := 0 ;
        FFLAG := 0
      end (* LOOK_FOR_CONSTANT *) ;


   begin (* MAIN_SCAN *)                                                              (*EL*)
     1 :
     READ_UNTIL_NEXT_DELImiter ;
     2 :
     if NFLAG <> 0 then
       if KFLAG = 0 then
         LOOK_FOR_NAME
       else
         LOOK_FOR_CONSTANT
     else
       begin
         JFLAG := 0 ;
         PFLAG := 0 ;
         FFLAG := 0
       end (* else *) ;
     3 :
     if DL <= 65 then
       goto 64 ;                 (*+,-*)
                                 (*EH *)
     if DL <= 68 then
       goto 66 ;                 (**,/,_:*)
     if DL <= 69 then
       goto 69 ;                 (*|^*)
     if DL <= 75 then
       goto 70 ;                 (*<,_<,=,_>,>,|=*)
     if DL <= 80 then
       goto 76 ;                 (*~,^,‘,=>,_=*)
     case DL of
       81 : goto 81 ; (*goto*)
                      (*KR  *)
       82 : goto 82 ; (*if*)
                      (*EY*)
       83 : goto 83 ; (*then*)
                      (*EN  *)
       84 : goto 84 ; (*else*)
                      (*FZ  *)
       85 : goto 85 ; (*for*)
                      (*FE *)
       86 : goto 86 ; (*do*)
                      (*FL*)
       87 : goto 87 ; (*, *)
                      (*EK*)
       90 : goto 90 ; (*: *)
                      (*FN*)
       91 : goto 91 ; (*; *)
                      (*FS*)
       92 : goto 92 ; (*:=*)
                      (*EZ*)
       94 : goto 94 ; (*step*)
                      (*FH  *)
       95 : goto 95 ; (*until*)
                      (*FK   *)
       96 : goto 96 ; (*while*)
                      (*FF   *)
       98 : goto 98 ; (*( *)
                      (*EW*)
       99 : goto 99 ; (*) *)
                      (*EU*)
       100 : goto 100 ; (*[ *)
                      (*EE*)
       101 : goto 101 ; (*] *)
                      (*EF*)
       102 : goto 102 ; (*|<*)
                      (*KS*)
       104 : goto 104 ; (*begin*)
                      (*LZ   *)
       105 : goto 105 ; (*end*)
                      (*FS *)
       106 : goto 106 ; (*own*)
                      (*KH *)
       107 : goto 107 ; (*Boolean*)
                      (*KZ     *)
       108 : goto 108 ; (*integer*)
                      (*KZ     *)
       109 : goto 109 ; (*real*)
                      (*KE  *)
       110 : goto 110 ; (*array*)
                      (*KF   *)
       111 : goto 111 ; (*switch*)
                      (*HE    *)
       112 : goto 112 ; (*procedure*)
                      (*HY       *)
     end (* case *) ;
     64 : (*+,-*)
         (*ES *)
     if OFLAG = 0 then
       begin
         PRODUCTION_OF_OBJECT_program ( 9 ) ;
         FILL_T_LIST_WITH_DELimiter
       end (* then *)
     else
       if DL = 65     (*-*)
       then
         begin
           OH := 10 ;
           DL := 132  (*NEG*) ;
           FILL_T_LIST_WITH_DELimiter
         end (* then *) ;
     goto 1 ;
     66 : (**,/,_:*)
         (*ET    *)
     PRODUCTION_OF_OBJECT_program ( 10 ) ;
     FILL_T_LIST_WITH_DELimiter ;
     goto 1 ;
     69 : (*|^*)
         (*KT*)
     PRODUCTION_OF_OBJECT_program ( 11 ) ;
     FILL_T_LIST_WITH_DELimiter ;
     goto 1 ;
     70 : (*<,_<,=,_>,>,|=*)
         (*KK            *)
     OFLAG := 1 ;
     PRODUCTION_OF_OBJECT_program ( 8 ) ;
     FILL_T_LIST_WITH_DELimiter ;
     goto 1 ;
     76 : (*~,^,‘,=>,_=*)
         (*KL         *)
     if DL = 76 (*~*)
     then
       begin
         OH := 83 - DL ;
         goto 8202
       end (* then *) ;
     PRODUCTION_OF_OBJECT_program ( 83 - DL ) ;
     FILL_T_LIST_WITH_DELimiter ;
     goto 1 ;
     81 : (*goto*)
         (*KR  *)
     RESERVATION_OF_ARRAYs ;
     goto 1 ;
     82 : (*if*)
         (*EY*)
     if EFLAG = 0 then
       RESERVATION_OF_ARRAYs ;
     FILL_T_LIST ( EFLAG ) ;
     EFLAG := 1 ;
     8201 :
     OH := 0 ;
     8202 :
     FILL_T_LIST_WITH_DELimiter ;
     OFLAG := 1 ;
     goto 1 ;
     83 : (*then*)
         (*EN  *)
     repeat
       PRODUCTION_OF_OBJECT_program ( 1 )
     until not THENELSE ;
     TLSC := TLSC - 1 ;
     EFLAG := STORE [ TLSC - 1 ] ;
     FILL_RESULT_LIST ( 30  (*CAC*) , 0 ) ;
     FILL_RESULT_LIST ( 2 , 88178688 + FLSC )  (*N 2T 'flsc'*) ;
     8301 :
     FILL_T_LIST ( FLSC ) ;
     FLSC := FLSC + 1 ;
     goto 8201 ;
     84 : (*else*)
         (*FZ  *)
     PRODUCTION_OF_OBJECT_program ( 1 ) ;
     if STORE [ TLSC - 1 ] MOD D8 = 84 (*else*)
     then
       if THENELSE then
         goto 84 ;
     8401 :
     if DO_IN_T_LIST then
       goto 8401 ;
     if STORE [ TLSC - 1 ] = 161 (*block-begin marker*)
     then
       begin
         TLSC := TLSC - 3 ;
         NLSC := STORE [ TLSC + 1 ] ;
         FILL_FUTURE_LIST ( FLIB + STORE [ TLSC ] , RLSC + 1 ) ;
         FILL_RESULT_LIST ( 12  (*RET*) , 0 ) ;
         BN := BN - 1 ;
         goto 8401
       end (* then *) ;
     FILL_RESULT_LIST ( 2 , 88080384 + FLSC )  (*2T 'flsc'*) ;
     if THENELSE     (*finds 'then'!*)
     then
       TLSC := TLSC + 1  (*keep eflag in t_list*) ;
     goto 8301 ;
     85 : (*for*)
         (*FE *)
     RESERVATION_OF_ARRAYs ;
     FILL_RESULT_LIST ( 2 , 88080384 + FLSC )  (*2T 'flsc'*) ;
     FORA := FLSC ;
     FLSC := FLSC + 1 ;
     FILL_T_LIST ( RLSC ) ;
     VFLAG := 1 ;
     BN := BN + 1 ;
     8501 :
     OH := 0 ;
     FILL_T_LIST_WITH_DELimiter ;
     goto 1 ;
     86 : (*do*)
         (*FL*)
     EMPTY_T_LIST_THROUGH_thenelse ;
     goto 8701 ;    (*execute part of DDEL ,*)
     8601 :         (*returned from DDEL ,*)
     VFLAG := 0 ;
     TLSC := TLSC - 1 ;
     FILL_RESULT_LIST ( 2 , 20971520 + FLSC )  (*2S 'flsc'*) ;
     FILL_T_LIST ( FLSC ) ;
     FLSC := FLSC + 1 ;
     FILL_RESULT_LIST ( 27  (*FOR8*) , 0 ) ;
     FILL_FUTURE_LIST ( FLIB + FORA , RLSC ) ;
     FILL_RESULT_LIST ( 19  (*FOR0*) , 0 ) ;
     FILL_RESULT_LIST ( 1 , 88604672  (*2T 0X0 A*) + STORE [ TLSC - 2 ]
                        ) ;
     FILL_FUTURE_LIST ( FLIB + FORC , RLSC ) ;
     EFLAG := 0 ;
     INTRO_NEW_BLOCK1 ;
     goto 8501 ;
     87 : (*, *)
         (*EK*)
     OFLAG := 1 ;
     if IFLAG = 1 then
       begin       (*subscript separator:*)
         repeat
           PRODUCTION_OF_OBJECT_program ( 1 )
         until not THENELSE ;
         goto 1
       end (* then *) ;
     if VFLAG = 0 then
       goto 8702 ;

     (*********************)
     (*for-list separator:*)
     (*********************)

     repeat
       PRODUCTION_OF_OBJECT_program ( 1 )
     until not THENELSE ;
     8701 :
     if STORE [ TLSC - 1 ] MOD D8 = 85 (*for*)
     then
       FILL_RESULT_LIST ( 21  (*for2*) , 0 )
     else
       begin
         TLSC := TLSC - 1 ;
         if STORE [ TLSC ] MOD D8 = 96   (*while*)
         then
           FILL_RESULT_LIST ( 23  (*for4*) , 0 )
         else
           FILL_RESULT_LIST ( 26  (*for7*) , 0 )
       end (* else *) ;
     if DL = 86  (*do*) then
       goto 8601 ;
     goto 1 ;
     8702 :
     if MFLAG = 0 then
       goto 8705 ;

     (*****************************)
     (*actual parameter separator:*)
     (*****************************)

     if STORE [ TLSC - 1 ] MOD D8 = 87 (*,*)
     then
       if AFLAG = 0 then
         if ( STORE [ TLSC - 2 ] = RLSC ) and ( FFLAG = 0 ) and ( JFLAG
         = 0 ) and ( NFLAG = 1 ) then
           begin
             if NID > NLSCOP then
               begin
                 if ( PFLAG = 1 ) and ( FFLAG = 0 ) then

     (***********************)
     (*non-formal procedure:*)
     (***********************)

                   TEST_FIRST_OCCURRENCe ;

     (********************)
     (*PORD construction:*)
     (********************)

                 if ( ID DIV D15 ) MOD 2 = 0 then
                   begin                   (*static addressing*)
                     PSTB := ( ( ID DIV D24 ) MOD D2 ) * D24 + ID MOD
                             D15 ;
                     if ( ID DIV D24 ) MOD D2 = 2 then
                       PSTB := PSTB + D17
                   end (* then *)
                 else
                   begin                  (*dynamic addressing*)
                     PSTB := D16 + ( ID MOD D5 ) * D22 + ( ID DIV D5 )
                             MOD D10 ;
                     if ( ID DIV D16 ) MOD 2 = 1 then
                       begin
                         STORE [ TLSC - 2 ] := PSTB + D17 ;
                         goto 8704
                       end (* then *)
                   end (* else *) ;
                 if ( ID DIV D18 ) MOD 2 = 1 then
                   STORE [ TLSC - 2 ] := PSTB + D20
                 else
                   if ( ID DIV D19 ) MOD 2 = 1 then
                     STORE [ TLSC - 2 ] := PSTB + D19
                   else
                     STORE [ TLSC - 2 ] := PSTB ;
                 goto 8704
               end (* then *)
             else
               begin
                 FILL_RESULT_LIST ( 98  (*TFP*) , 0 ) ;
                 goto 8703
               end (* else *)
           end (* then *)
         else
           goto 8703
       else
         begin          (*completion of implicit subroutine:*)
           STORE [ TLSC - 2 ] := STORE [ TLSC - 2 ] + D19 + D20 + D24 ;
           FILL_RESULT_LIST ( 13  (*EIS*) , 0 ) ;
           goto 8704
         end (* else *) ;
     8703 : (*completion of implicit subroutine:*)
     repeat
       PRODUCTION_OF_OBJECT_program ( 1 )
     until not ( THENELSE or DO_IN_T_LIST ) ;
     STORE [ TLSC - 2 ] := STORE [ TLSC - 2 ] + D20 + D24 ;
     FILL_RESULT_LIST ( 13  (*EIS*) , 0 ) ;
     8704 :
     if DL = 87  (*,*) then
       goto 9804  (*prepare next parameter*) ;

     (**********************)
     (*production of PORDs:*)
     (**********************)

     PSTA := 0 ;
     UNLOAD_T_LIST_ELEMENt ( PSTB ) ;
     while PSTB MOD D8 = 87  (*,*) do
       begin
         PSTA := PSTA + 1 ;
         UNLOAD_T_LIST_ELEMENt ( PSTB ) ;
         if PSTB DIV D16 MOD 2 = 0 then
           FILL_RESULT_LIST ( PSTB DIV D24 , PSTB MOD D24 )
         else
           FILL_RESULT_LIST ( 0 , PSTB ) ;
         UNLOAD_T_LIST_ELEMENt ( PSTB )
       end (* while *) ;
     TLSC := TLSC - 1 ;
     FILL_FUTURE_LIST ( FLIB + STORE [ TLSC ] , RLSC ) ;
     FILL_RESULT_LIST ( 0 , 4718592 + PSTA )  (*2A 'psta' A*) ;
     BN := BN - 1 ;
     UNLOAD_T_LIST_ELEMENt ( FFLAG ) ;
     UNLOAD_T_LIST_ELEMENt ( EFLAG ) ;
     PRODUCTION_TRANSMARK ;
     AFLAG := 0 ;
     UNLOAD_T_LIST_ELEMENt ( MFLAG ) ;
     UNLOAD_T_LIST_ELEMENt ( VFLAG ) ;
     UNLOAD_T_LIST_ELEMENt ( IFLAG ) ;
     goto 1 ;
     8705 :
     EMPTY_T_LIST_THROUGH_thenelse ;
     if SFLAG = 0 then  (*array declaration*)
       goto 1 ;

     (*********************)
     (*switch declaration:*)
     (*********************)

     OH := 0 ;
     DL := 160 ;
     FILL_T_LIST ( RLSC ) ;
     FILL_T_LIST_WITH_DELimiter ;
     goto 1 ;
     90 : (*: *)
         (*FN*)
     if JFLAG = 0 then
       begin      (*array declaration*)
         IC := IC + 1 ;
         EMPTY_T_LIST_THROUGH_thenelse
       end (* then *)
     else
       begin      (*label declaration*)
         RESERVATION_OF_ARRAYs ;
         LABEL_DECLARATION
       end (* else *) ;
     goto 1 ;
     91 :
     goto 105  (*end*) ;
     92 : (*:=*)
         (*EZ*)
     RESERVATION_OF_ARRAYs ;
     DL := 128  (*ST*) ;
     OFLAG := 1 ;
     if VFLAG = 0 then
       begin
         if SFLAG = 0 then
           begin         (*assignment statement*)
             if EFLAG = 0 then
               EFLAG := 1
             else
               DL := 129  (*STA*) ;
             OH := 2 ;
             if PFLAG = 0 then
               begin            (*assignment to variable*)
                 if NFLAG <> 0 then  (*assignment to scalar*)
                   GENERATE_ADDRESS ;
               end (* then *)
             else
               begin            (*assignment to function identifier*)
                 DL := DL + 2  (*STP or STAP*) ;
                 FILL_T_LIST ( ( ID DIV D19 ) MOD D5  (*bn from id*) )
               end (* else *) ;
             FILL_T_LIST_WITH_DELimiter
           end (* then *)
         else
           begin         (*switch declaration*)
             FILL_RESULT_LIST ( 2 , 88080384 + FLSC )  (*2T 'flsc'*) ;
             FILL_T_LIST ( FLSC ) ;
             FLSC := FLSC + 1 ;
             FILL_T_LIST ( NID ) ;
             OH := 0 ;
             FILL_T_LIST_WITH_DELimiter ;
             DL := 160 ;
             FILL_T_LIST ( RLSC ) ;
             FILL_T_LIST_WITH_DELimiter
           end (* else *)
       end (* then *)
     else
       begin      (*for statement*)
         EFLAG := 1 ;
         if NFLAG <> 0 then  (*simple variable*)
           GENERATE_ADDRESS ;
         FILL_RESULT_LIST ( 20  (*FOR1*) , 0 ) ;
         FORC := FLSC ;
         FILL_RESULT_LIST ( 2 , 88080384 + FLSC )  (*2T 'flsc'*) ;
         FLSC := FLSC + 1 ;
         FILL_FUTURE_LIST ( FLIB + FORA , RLSC ) ;
         FILL_RESULT_LIST ( 0 , 4718592  (*2A 0 A*) ) ;
         FORA := FLSC ;
         FILL_RESULT_LIST ( 2 , 71303168 + FLSC )  (*2B 'flsc*) ;
         FLSC := FLSC + 1 ;
         FILL_RESULT_LIST ( 9  (*ETMP*) , 0 )
       end (* else *) ;
     goto 1 ;
     94 : (*step*)
         (*FH  *)
     EMPTY_T_LIST_THROUGH_thenelse ;
     FILL_RESULT_LIST ( 24  (*FOR5*) , 0 ) ;
     goto 1 ;
     95 : (*until*)
         (*FK   *)
     EMPTY_T_LIST_THROUGH_thenelse ;
     FILL_RESULT_LIST ( 25  (*FOR6*) , 0 ) ;
     goto 8501 ;
     96 : (*while*)
         (*FF   *)
     EMPTY_T_LIST_THROUGH_thenelse ;
     FILL_RESULT_LIST ( 22  (*FOR3*) , 0 ) ;
     goto 8501 ;
     98 : (*( *)
         (*EW*)
     OFLAG := 1 ;
     if PFLAG = 1 then
       goto 9803 ;
     9801 : (*parenthesis in expression:*)
     FILL_T_LIST ( MFLAG ) ;
     MFLAG := 0 ;
     9802 :
     OH := 0 ;
     FILL_T_LIST_WITH_DELimiter ;
     goto 1 ;
     9803 : (*begin of parameter list:*)
     PROCEDURE_STATEMENT ;
     FILL_RESULT_LIST ( 2 , 88080384 + FLSC )  (*2T 'flsc'*) ;
     FILL_T_LIST ( IFLAG ) ;
     FILL_T_LIST ( VFLAG ) ;
     FILL_T_LIST ( MFLAG ) ;
     FILL_T_LIST ( EFLAG ) ;
     FILL_T_LIST ( FFLAG ) ;
     FILL_T_LIST ( FLSC ) ;
     IFLAG := 0 ;
     VFLAG := 0 ;
     MFLAG := 1 ;
     EFLAG := 1 ;
     FLSC := FLSC + 1 ;
     OH := 0 ;
     BN := BN + 1 ;
     FILL_T_LIST_WITH_DELimiter ;
     DL := 87  (*,*) ;
     9804 : (*prepare parsing of actual parameter:*)
     FILL_T_LIST ( RLSC ) ;
     AFLAG := 0 ;
     goto 9802 ;
     99 : (*) *)
         (*EU*)
     if MFLAG = 1 then
       goto 8702 ;
     repeat
       PRODUCTION_OF_OBJECT_program ( 1 )
     until not THENELSE ;
     TLSC := TLSC - 1 ;
     UNLOAD_T_LIST_ELEMENt ( MFLAG ) ;
     goto 1 ;
     100 : (*[ *)
          (*EE*)
     if EFLAG = 0 then
       RESERVATION_OF_ARRAYs ;
     OFLAG := 1 ;
     OH := 0 ;
     FILL_T_LIST ( EFLAG ) ;
     FILL_T_LIST ( IFLAG ) ;
     FILL_T_LIST ( MFLAG ) ;
     FILL_T_LIST ( FFLAG ) ;
     FILL_T_LIST ( JFLAG ) ;
     FILL_T_LIST ( NID ) ;
     EFLAG := 1 ;
     IFLAG := 1 ;
     MFLAG := 0 ;
     FILL_T_LIST_WITH_DELimiter ;
     if JFLAG = 0 then
       GENERATE_ADDRESS  (*of storage function*) ;
     goto 1 ;
     101 : (*] *)
          (*EF*)
     repeat
       PRODUCTION_OF_OBJECT_program ( 1 )
     until not THENELSE ;
     TLSC := TLSC - 1 ;
     if IFLAG = 0 then
       begin      (*array declaration:*)
         FILL_RESULT_LIST ( 0 , 21495808 + AIC  (*2S 'aic' A*) ) ;
         FILL_RESULT_LIST ( 90  (*RSF*) + IBD , 0 )  (*RSF or ISF*) ;
         ARRB := D15 + D25 + D26 ;
         if IBD = 1 then
           ARRB := ARRB + D19 ;
         ARRA := NLIB + NLSC ;
         repeat
           STORE [ ARRA - 1 ] := ARRB + PNLV ;
           if STORE [ ARRA - 2 ] MOD D3 = 0 then
             ARRA := ARRA - 2
           else
             ARRA := ARRA - 3 ;
           PNLV := PNLV + ( IC + 3 ) * D5 ;
           AIC := AIC - 1
         until AIC = 0 ;
         READ_UNTIL_NEXT_DELImiter ;
         if DL <> 91 then
           goto 1103 ;
         EFLAG := 0 ;
         goto 1
       end (* then *) ;
     UNLOAD_T_LIST_ELEMENt ( NID ) ;
     UNLOAD_T_LIST_ELEMENt ( JFLAG ) ;
     UNLOAD_T_LIST_ELEMENt ( FFLAG ) ;
     UNLOAD_T_LIST_ELEMENt ( MFLAG ) ;
     UNLOAD_T_LIST_ELEMENt ( IFLAG ) ;
     UNLOAD_T_LIST_ELEMENt ( EFLAG ) ;
     if JFLAG = 0 then
       begin      (*subscripted variable:*)
         AFLAG := 1 ;
         FILL_RESULT_LIST ( 56  (*IND*) , 0 ) ;
         goto 1
       end (* then *) ;

     (********************)
     (*switch designator:*)
     (********************)

     NFLAG := 1 ;
     FILL_RESULT_LIST ( 29  (*SSI*) , 0 ) ;
     READ_NEXT_SYMBOL ;
     ID := STORE [ NLIB + NID ] ;
     PFLAG := 0 ;
     goto 3 ;
     102 : (*|<*)
          (*KS*)
     QC := 1 ;
     QB := 0 ;
     QA := 1 ;
     repeat
       READ_NEXT_SYMBOL ;
       if DL = 102  (*|<*) then
         QC := QC + 1 ;
       if DL = 103  (*|>*) then
         QC := QC - 1 ;
       if QC > 0 then
         begin
           QB := QB + DL * QA ;
           QA := QA * D8 ;
           if QA = D24 then
             begin
               FILL_RESULT_LIST ( 0 , QB ) ;
               QB := 0 ;
               QA := 1
             end (* then *)
         end (* then *)
     until QC = 0 ;
     FILL_RESULT_LIST ( 0 , QB + 255  (*end marker*) * QA ) ;
     OFLAG := 0 ;
     goto 1 ;
     104 : (*begin*)
          (*LZ   *)
     if STORE [ TLSC - 1 ] <> 161 (*block-begin marker*)
     then
       RESERVATION_OF_ARRAYs ;
     goto 8501 ;
     105 : (*end*)
          (*FS *)
     RESERVATION_OF_ARRAYs ;
     repeat
       EMPTY_T_LIST_THROUGH_thenelse
     until not DO_IN_T_LIST ;
     if SFLAG = 0 then
       begin
         if STORE [ TLSC - 1 ] = 161     (*blok-begin marker*)
         then
           begin
             TLSC := TLSC - 3 ;
             NLSC := STORE [ TLSC + 1 ] ;
             FILL_FUTURE_LIST ( FLIB + STORE [ TLSC ] , RLSC + 1 ) ;
             FILL_RESULT_LIST ( 12  (*RET*) , 0 ) ;
             BN := BN - 1 ;
             goto 105
           end (* then *)
       end (* then *)
     else
       begin      (*end of switch declaration*)
         SFLAG := 0 ;
         repeat
           TLSC := TLSC - 2 ;
           FILL_RESULT_LIST ( 1 , 88604672 + STORE [ TLSC ] )

     (*********************)
     (*2T 'stacked RLSC' A*)
     (*********************)

         until STORE [ TLSC - 1 ] <> 160  (*switch comma*) ;
         TLSC := TLSC - 1 ;
         UNLOAD_T_LIST_ELEMENt ( NID ) ;
         LABEL_DECLARATION ;
         FILL_RESULT_LIST ( 0 , 85983232 + 48 )  (*1T 16X1*) ;
         TLSC := TLSC - 1 ;
         FILL_FUTURE_LIST ( FLIB + STORE [ TLSC ] , RLSC )
       end (* else *) ;
     EFLAG := 0 ;
     if DL <> 105  (*end*) then
       goto 1 ;
     TLSC := TLSC - 1 ;
     if TLSC = TLIB + 1 then
       goto 1052 ;
     repeat
       READ_NEXT_SYMBOL
     until ( DL = 91  (*;*) ) or ( DL = 84  (*else*) ) or ( DL = 105

     (*****)
     (*end*)
     (*****)

     ) ;
     JFLAG := 0 ;
     PFLAG := 0 ;
     FFLAG := 0 ;
     NFLAG := 0 ;
     goto 2 ;
     106 : (*own*)
          (*KH *)
     NEW_BLOCK_BY_DECL ;
     READ_NEXT_SYMBOL ;
     if DL = 109  (*real*) then
       IBD := 0
     else
       IBD := 1 ;
     READ_UNTIL_NEXT_DELImiter ;
     if NFLAG = 0 then
       goto 1102 ;
     goto 1082 ;
     107 : (*Boolean*)
          (*KZ     *)
     goto 108  (*integer*) ;
     108 : (*integer*)
          (*KZ     *)
     IBD := 1 ;
     NEW_BLOCK_BY_DECL ;
     READ_UNTIL_NEXT_DELImiter ;
     1081 :
     if NFLAG = 0 then
       begin
         if DL = 110  (*array*) then
           goto 1101 ;
         goto 112      (*procedure*)
       end (* then *) ;

     (*********)
     (*scalar:*)
     (*********)

     if BN <> 0 then
       goto 1083 ;
     1082 : (*static addressing*)
     ID := GVC ;
     if IBD = 1 then
       begin
         ID := ID + D19 ;
         GVC := GVC + 1
       end (* then *)
     else
       GVC := GVC + 2 ;
     FILL_NAME_LIST ;
     if DL = 87   (*,*)
     then
       begin
         READ_UNTIL_NEXT_DELImiter ;
         goto 1082
       end (* then *) ;
     goto 1 ;
     1083 : (*dynamic addressing*)
     ID := PNLV + D15 ;
     if IBD = 1 then
       begin
         ID := ID + D19 ;
         PNLV := PNLV + 32 ;
         LVC := LVC + 1
       end (* then *)
     else
       begin
         PNLV := PNLV + 2 * 32 ;
         LVC := LVC + 2
       end (* else *) ;
     FILL_NAME_LIST ;
     if DL = 87   (*,*)
     then
       begin
         READ_UNTIL_NEXT_DELImiter ;
         goto 1083
       end (* then *) ;
     READ_UNTIL_NEXT_DELImiter ;
     if ( DL <= 106  (*own*) ) or ( DL > 109  (*real*) ) then
       begin
         RESERVATION_OF_LOCAL_variables ;
         goto 2
       end (* then *) ;
     if DL = 109  (*real*) then
       IBD := 0
     else
       IBD := 1 ;
     READ_UNTIL_NEXT_DELImiter ;
     if NFLAG = 1 then
       goto 1083  (*more scalars*) ;
     RESERVATION_OF_LOCAL_variables ;
     if DL = 110  (*array*) then
       goto 1101 ;
     goto 3 ;
     109 : (*real*)
          (*KE  *)
     IBD := 0 ;
     NEW_BLOCK_BY_DECL ;
     READ_UNTIL_NEXT_DELImiter ;
     if NFLAG = 1 then
       goto 1081 ;
     goto 2 ;
     110 : (*array*)
          (*KF   *)
     IBD := 0 ;
     NEW_BLOCK_BY_DECL ;
     1101 :
     if BN <> 0 then
       goto 1103 ;
     1102 : (*static bounds, constants only:*)
     ID := 3 * D24 ;
     if IBD <> 0 then
       ID := ID + D19 ;
     repeat
       ARRA := NLSC ;
       ARRB := TLSC ;
       repeat    (*read identifier list:*)
         READ_UNTIL_NEXT_DELImiter ;
         FILL_NAME_LIST
       until DL = 100  (*[*) ;
       ARRC := 0 ;
       FILL_T_LIST ( 2 - IBD ) ; (*delta[0]*)
       repeat    (*read bound-pair list:*)
                 (*lower bound:         *)
         READ_UNTIL_NEXT_DELImiter ;
         if DL <> 90    (*:*)
         then
           if DL = 64   (*+*)
           then
             begin
               READ_UNTIL_NEXT_DELImiter ;
               ARRD := INW
             end (* then *)
           else
             begin
               READ_UNTIL_NEXT_DELImiter ;
               ARRD := - INW
             end (* else *)
         else
           ARRD := INW ;
         ARRC := ARRC - ( ARRD * STORE [ TLSC - 1 ] ) MOD D26 ;

     (**************)
     (*upper bound:*)
     (**************)

         READ_UNTIL_NEXT_DELImiter ;
         if NFLAG = 0 then
           if DL = 65      (*-*)
           then
             begin
               READ_UNTIL_NEXT_DELImiter ;
               ARRD := - INW - ARRD
             end (* then *)
           else
             begin
               READ_UNTIL_NEXT_DELImiter ;
               ARRD := INW - ARRD
             end (* else *)
         else
           ARRD := INW - ARRD ;
         if DL = 101   (*[*)
         then
           FILL_T_LIST ( - ( ( ARRD + 1 ) * STORE [ TLSC - 1 ] ) MOD
                         D26 )
         else
           FILL_T_LIST ( ( ( ARRD + 1 ) * STORE [ TLSC - 1 ] ) MOD D26
                         )
       until DL = 101  (*]*) ;
       ARRD := NLSC ;
       repeat    (*construction of storage function in constant list:*)
         STORE [ NLIB + ARRD - 1 ] := STORE [ NLIB + ARRD - 1 ] + KLSC
                                      ;
         FILL_CONSTANT_LIST ( GVC ) ;
         FILL_CONSTANT_LIST ( GVC + ARRC ) ;
         TLSC := ARRB ;
         repeat
           FILL_CONSTANT_LIST ( STORE [ TLSC ] ) ;
           TLSC := TLSC + 1
         until STORE [ TLSC - 1 ] <= 0 ;
         GVC := GVC - STORE [ TLSC - 1 ] ;
         TLSC := ARRB ;
         if STORE [ NLIB + ARRD - 2 ] MOD D3 = 0 then
           ARRD := ARRD - 2
         else
           ARRD := ARRD - 3
       until ARRD = ARRA ;
       READ_UNTIL_NEXT_DELImiter
     until DL <> 87  (*,*) ;
     goto 91  (*;*) ;
     1103 : (*dynamic bounds,arithmetic expressions:*)
     IC := 0 ;
     AIC := 0 ;
     ID := 0 ;
     repeat
       AIC := AIC + 1 ;
       READ_UNTIL_NEXT_DELImiter ;
       FILL_NAME_LIST
     until DL <> 87  (*,*) ;
     EFLAG := 1 ;
     OFLAG := 1 ;
     goto 8501 ;
     111 : (*switch*)
          (*HE    *)
     RESERVATION_OF_ARRAYs ;
     SFLAG := 1 ;
     NEW_BLOCK_BY_DECL ;
     goto 1 ;
     112 : (*procedure*)
          (*HY       *)
     RESERVATION_OF_ARRAYs ;
     NEW_BLOCK_BY_DECL ;
     FILL_RESULT_LIST ( 2 , 88080384 + FLSC )  (*2T 'flsc'*) ;
     FILL_T_LIST ( FLSC ) ;
     FLSC := FLSC + 1 ;
     READ_UNTIL_NEXT_DELImiter ;
     LOOK_FOR_NAME ;
     LABEL_DECLARATION ;
     INTRO_NEW_BLOCK ;
     NEW_BLOCK_BY_DECL1 ;
     if DL = 91  (*;*) then
       goto 1 ;

     (************************)
     (*formal parameter list:*)
     (************************)

     repeat
       READ_UNTIL_NEXT_DELImiter ;
       ID := PNLV + D15 + D16 ;
       FILL_NAME_LIST ;
       PNLV := PNLV + 2 * D5                   (*reservation PARD*)
     until DL <> 87 ;
     READ_UNTIL_NEXT_DELImiter ;   (*for ; after )*)
     1121 :
     READ_UNTIL_NEXT_DELImiter ;
     if NFLAG = 1 then
       goto 2 ;
     if DL = 104  (*begin*) then
       goto 3 ;
     if DL <> 115  (*value*) then
       goto 1123  (*specification part*) ;

     (*************)
     (*value part:*)
     (*************)

     SPE := D26 ;  (*value flag*)
     1122 :
     repeat
       READ_UNTIL_NEXT_DELImiter ;
       LOOK_FOR_NAME ;
       STORE [ NLIB + NID ] := STORE [ NLIB + NID ] + SPE
     until DL <> 87 ;
     goto 1121 ;
     1123 : (*specification part:*)
     if ( DL = 113  (*string*) ) or ( DL = 110  (*array*) ) then
       begin
         SPE := 0 ;
         goto 1122
       end (* then *) ;
     if ( DL = 114  (*label*) ) or ( DL = 111  (*switch*) ) then
       begin
         SPE := D17 ;
         goto 1122
       end (* then *) ;
     if DL = 112   (*procedure*)
     then
       begin
         SPE := D18 ;
         goto 1122
       end (* then *) ;
     if DL = 109   (*real*)
     then
       SPE := 0
     else
       SPE := D19 ;
     if ( DL <= 106 ) or ( DL > 109 ) then
       goto 3 ;                                   (*if,for,goto*)
     READ_UNTIL_NEXT_DELImiter ;

     (**********************************************)
     (*for delimiter following real/integer/boolean*)
     (**********************************************)

     if DL = 112   (*procedure*)
     then
       begin
         SPE := D18 ;
         goto 1122
       end (* then *) ;
     if DL = 110  (*array*) then
       goto 1122 ;
     1124 :
     LOOK_FOR_NAME ;
     STORE [ NLIB + NID ] := STORE [ NLIB + NID ] + SPE ;
     if STORE [ NLIB + NID ] >= D26 then
       begin
         ID := STORE [ NLIB + NID ] - D26 ;
         ID := ( ID DIV D17 ) * D17 + ID MOD D16 ;
         STORE [ NLIB + NID ] := ID ;
         ADDRESS_TO_REGISTER ;      (*generates 2S 'PARD position' A*)
         if SPE = 0 then
           FILL_RESULT_LIST ( 14  (*TRAD*) , 0 )
         else
           FILL_RESULT_LIST ( 16  (*TIAD*) , 0 ) ;
         ADDRESS_TO_REGISTER ;      (*generates 2S 'PARD position' A*)
         FILL_RESULT_LIST ( 35  (*TFR*) , 0 ) ;
         FILL_RESULT_LIST ( 85  (*ST*) , 0 )
       end (* then *) ;
     if DL = 87   (*,*)
     then
       begin
         READ_UNTIL_NEXT_DELImiter ;
         goto 1124
       end (* then *) ;
     goto 1121 ;
     1052 :
     
   end (* MAIN_SCAN *) ;



procedure PROGRAM_LOADER ;                                            (*RZ*)

   var I , J , LL , LIST_ADDRESS , ID , MCP_COUNT , CRFA : INTEGER ;
       HEPTADE_COUNT , PARITY_WORD , READ_LOCATION , STOCK : INTEGER ;
       FROM_STORE : 0 .. 1 ;
       USE : BOOLEAN ;


   function LOGICAL_SUM ( N , M : INTEGER ) : INTEGER ;

   (************************************)
   (*emulation of a machine instruction*)
   (************************************)


      var I , W : INTEGER ;

      begin (* LOGICAL_SUM *)
        W := 0 ;
        for I := 0 to 26 do
          begin
            W := W DIV 2 ;
            if N MOD 2 = M MOD 2 then
              W := W + D26 ;
            N := N DIV 2 ;
            M := M DIV 2
          end (* for *) ;
        LOGICAL_SUM := W
      end (* LOGICAL_SUM *) ;


   procedure COMPLETE_BITSTOCK ;                                      (*RW*)

      var I , W : INTEGER ;

      begin (* COMPLETE_BITSTOCK *)
        while BITCOUNT > 0  (*i.e., at most 20 bits in stock*) do
          begin
            HEPTADE_COUNT := HEPTADE_COUNT + 1 ;
            case FROM_STORE of
              0 : (*bit string read from store:*)
                  begin
                    if HEPTADE_COUNT > 0 then
                      begin
                        BITCOUNT := BITCOUNT + 1 ;
                        HEPTADE_COUNT := - 3 ;
                        READ_LOCATION := READ_LOCATION - 1 ;
                        STOCK := STORE [ READ_LOCATION ] ;
                        W := STOCK DIV D21 ;
                        STOCK := ( STOCK MOD D21 ) * 64
                      end (* then *)
                    else
                      begin
                        W := STOCK DIV D20 ;
                        STOCK := ( STOCK MOD D20 ) * 128
                      end (* else *)
                  end (* tag/ca *) ;
              1 : (*bit string read from tape:*)
                  begin
                    READ ( LIB_TAPE , W ) ;
                    if HEPTADE_COUNT > 0 then
                      begin (*test parity of the previous 4 heptades*)
                        BITCOUNT := BITCOUNT + 1 ;
                        PARITY_WORD := LOGICAL_SUM ( PARITY_WORD ,
                                       PARITY_WORD DIV D4 ) MOD D4 ;
                        if PARITY_WORD in [ 0 , 3 , 5 , 6 , 9 , 10 , 12
                        , 15 ] then
                          STOP ( 105 ) ;
                        HEPTADE_COUNT := - 3 ;
                        PARITY_WORD := W ;
                        W := W DIV 2
                      end (* then *)
                    else
                      PARITY_WORD := LOGICAL_SUM ( PARITY_WORD , W )
                  end (* tag/ca *)
            end (* case *) ;
            for I := 1 to BITCOUNT - 1 do
              W := 2 * W ;
            BITSTOCK := BITSTOCK + W ;
            BITCOUNT := BITCOUNT - 7
          end (* while *)
      end (* COMPLETE_BITSTOCK *) ;


   function READ_BIT_STRING ( N : INTEGER ) : INTEGER ;               (*RW*)

      var I , W : INTEGER ;

      begin (* READ_BIT_STRING *)
        W := 0 ;
        for I := 1 to N do
          begin
            W := 2 * W + BITSTOCK DIV D26 ;
            BITSTOCK := ( BITSTOCK MOD D26 ) * 2
          end (* for *) ;
        READ_BIT_STRING := W ;
        BITCOUNT := BITCOUNT + N ;
        COMPLETE_BITSTOCK
      end (* READ_BIT_STRING *) ;


   procedure PREP_READ_BITSTR1 ;

      var I : INTEGER ;

      begin (* PREP_READ_BITSTR1 *)
        for I := 1 to 27 - BITCOUNT do
          BITSTOCK := 2 * BITSTOCK ;
        BITCOUNT := 21 - BITCOUNT ;
        HEPTADE_COUNT := 0 ;
        FROM_STORE := 0 ;
        COMPLETE_BITSTOCK
      end (* PREP_READ_BITSTR1 *) ;


   procedure PREP_READ_BITSTR2 ;

      begin (* PREP_READ_BITSTR2 *)
        BITSTOCK := 0 ;
        BITCOUNT := 21 ;
        HEPTADE_COUNT := 0 ;
        FROM_STORE := 0 ;
        COMPLETE_BITSTOCK ;
        repeat
          
        until READ_BIT_STRING ( 1 ) = 1
      end (* PREP_READ_BITSTR2 *) ;


   procedure PREP_READ_BITSTR3 ;

      var W : INTEGER ;

      begin (* PREP_READ_BITSTR3 *)
        FROM_STORE := 1 ;
        BITSTOCK := 0 ;
        BITCOUNT := 21 ;
        repeat
          READ ( LIB_TAPE , W )
        until W <> 0 ;
        if W <> 30  (*D*) then
          STOP ( 106 ) ;
        HEPTADE_COUNT := 0 ;
        PARITY_WORD := 1 ;
        COMPLETE_BITSTOCK ;
        repeat
          
        until READ_BIT_STRING ( 1 ) = 1
      end (* PREP_READ_BITSTR3 *) ;


   function ADDRESS_DECODING : INTEGER ;                              (*RY*)

      var W , A , N : INTEGER ;

      begin (* ADDRESS_DECODING *)
        W := BITSTOCK ;
        if W < D26 (*code starts with 0*)
        then
          begin  (*0*)
            N := 1 ;
            A := 0 ;
            W := 2 * W
          end (* then *)
        else
          begin  (*1xxxxx*)
            N := 6 ;
            A := ( W DIV D21 ) MOD D5 ;
            W := ( W MOD D21 ) * D6
          end (* else *) ;
        if W < D25 (*00*)
        then
          begin  (*00*)
            N := N + 2 ;
            A := 32 * A + 0 ;
            W := W * 4
          end (* then *)
        else
          if W < D26 (*01*)
          then
            begin  (*01xx*)
              N := N + 4 ;
              A := 32 * A + W DIV D23 ;
              if A MOD D5 < 6 then  (*010x*)
                A := A - 3
              else  (*011x*)
                A := A - 2 ;
              W := ( W MOD D23 ) * D4
            end (* then *)
          else
            begin  (*1xxxxx*)
              N := N + 6 ;
              A := A * 32 + ( W DIV D21 ) MOD D5 ;
              W := ( W MOD D21 ) * D6
            end (* else *) ;
        if W < D25 (*00*)
        then
          begin  (*00*)
            N := N + 2 ;
            A := 32 * A + 1
          end (* then *)
        else
          if W < D26 (*01*)
          then
            begin  (*01x*)
              N := N + 3 ;
              A := 32 * A + W DIV D24
            end (* then *)
          else
            begin  (*1xxxxx*)
              N := N + 6 ;
              A := 32 * A + ( W DIV D21 ) MOD D5
            end (* else *) ;
        W := READ_BIT_STRING ( N ) ;
        ADDRESS_DECODING := A
      end (* ADDRESS_DECODING *) ;


   function READ_MASK : INTEGER ;                                     (*RN*)

      var C : 0 .. 19 ;

      begin (* READ_MASK *)
        if BITSTOCK < D26 (*code starts with 0*)
        then  (*0x*)
          C := READ_BIT_STRING ( 2 )
        else
          if BITSTOCK < D26 + D25 (*01*)
          then  (*10x*)
            C := READ_BIT_STRING ( 3 ) - 2
          else  (*11xxxx*)
            C := READ_BIT_STRING ( 6 ) - 44 ;
        case C of
          0 : READ_MASK := 656 ; (*0,   2S 0    A  *)
          1 : READ_MASK := 14480 ; (*3,   2B 0    A  *)
          2 : READ_MASK := 10880 ; (*2,   2T 0 X0    *)
          3 : READ_MASK := 2192 ; (*0,   2B 0    A  *)
          4 : READ_MASK := 144 ; (*0,   2A 0    A  *)
          5 : READ_MASK := 10368 ; (*2,   2B 0 X0    *)
          6 : READ_MASK := 6800 ; (*1,   2T 0    A  *)
          7 : READ_MASK := 0 ;  (*0,   0A 0 X0    *)
          8 : READ_MASK := 12304 ; (*3,   0A 0    A  *)
          9 : READ_MASK := 10883 ; (*2, N 2T 0 X0    *)
          10 : READ_MASK := 6288 ; (*1,   2B 0    A  *)
          11 : READ_MASK := 4128 ; (*1,   0A 0 X0 B  *)
          12 : READ_MASK := 8832 ; (*2,   2S 0 X0    *)
          13 : READ_MASK := 146 ; (*0, Y 2A 0    A  *)
          14 : READ_MASK := 256 ; (*0,   4A 0 X0    *)
          15 : READ_MASK := 134 ; (*0, Y 2A 0 X0   P*)
          16 : READ_MASK := 402 ; (*0, Y 6A 0    A  *)
          17 : READ_MASK := 4144 ; (*1,   0A 0 X0 C  *)
          18 : READ_MASK := 16 ; (*0,   0A 0    A  *)
          19 : READ_MASK := ADDRESS_DECODING
        end (* case *)
      end (* READ_MASK *) ;


   function READ_BINARY_WORD : INTEGER ;                              (*RF*)

      var W : INTEGER ;
          OPC : 0 .. 3 ;

      begin (* READ_BINARY_WORD *)
        if BITSTOCK < D26   (*code starts with 0*)
        then
          begin             (*OPC >= 8*)
            if BITSTOCK < D25 (*00*)
            then
              if BITSTOCK < D24 (*000*)
              then
                W := 4      (*code is 000x*)
              else
                W := 5      (*code is 001xx*)
            else
              if BITSTOCK < D25 + D24 (*010*)
              then
                if BITSTOCK < D25 + D23 (*0100*)
                then
                  W := 6    (*0100xx*)
                else
                  W := 7    (*0101xxx*)
              else
                W := 10  (*011xxxxxxx*) ;
            W := READ_BIT_STRING ( W ) ;
            if W < 2  (*000x*) then  (*no change*)
              
            else
              if W < 8  (*001xx*) then
                W := W - 2
              else
                if W < 24  (*010xx*) then
                  W := W - 10
                else
                  if W < 48  (*0101xxx*) then
                    W := W - 30
                  else  (*011xxxxxxx*)
                    W := W - 366 ;
            READ_BINARY_WORD := OPC_TABLE [ W ]
          end (* then *)
        else
          begin
            W := READ_BIT_STRING ( 1 ) ;
            W := READ_MASK ;
            OPC := W DIV D12 ;
            W := ( W MOD D12 ) * D15 + ADDRESS_DECODING ;
            case OPC of
              0 : ;
              1 : W := W + LIST_ADDRESS ;
              2 : begin
                    if W DIV D17 MOD 2 = 1     (*d17 = 1*)
                    then
                      W := W - D17
                    else
                      W := W + D19 ;
                    W := W - W MOD D15 + STORE [ FLIB + W MOD D15 ]
                  end (* tag/ca *) ;
              3 : if KLIB = CRFB then
                    W := W - W MOD D15 + STORE [ MLIB + W MOD D15 ]
                  else
                    W := W + KLIB
            end (* case *) ;
            READ_BINARY_WORD := W
          end (* else *)
      end (* READ_BINARY_WORD *) ;


   procedure TEST_BIT_STOCK ;                                         (*RH*)

      begin (* TEST_BIT_STOCK *)
        if BITSTOCK <> 63 * D21 then
          STOP ( 107 )
      end (* TEST_BIT_STOCK *) ;


   procedure TYP_ADDRESS ( A : INTEGER ) ;                            (*RT*)

      begin (* TYP_ADDRESS *)
        WRITELN ( OUTPUT ) ;
        WRITE ( OUTPUT , A DIV 1024 : 2 , ' ' , ( A MOD 1024 ) DIV 32 :
                2 , ' ' , A MOD 32 : 2 )
      end (* TYP_ADDRESS *) ;


   procedure READ_LIST ;                                              (*RL*)

      var I , J , W : INTEGER ;

      begin (* READ_LIST *)
        for I := LL - 1 DOWNTO 0 do
          begin
            W := READ_BINARY_WORD ;
            if LIST_ADDRESS + I <= FLIB + FLSC then
              begin (*shift FLI downwards*)
                if FLIB <= READ_LOCATION then
                  STOP ( 98 ) ;
                for J := 0 to FLSC - 1 do
                  STORE [ READ_LOCATION + J ] := STORE [ FLIB + J ] ;
                FLIB := READ_LOCATION
              end (* then *) ;
            STORE [ LIST_ADDRESS + I ] := W
          end (* for *) ;
        TEST_BIT_STOCK ;
      end (* READ_LIST *) ;


   function READ_CRF_ITEM : INTEGER ;                                 (*RS*)

      begin (* READ_CRF_ITEM *)
        if CRFA MOD 2 = 0 then
          READ_CRF_ITEM := STORE [ CRFA DIV 2 ] DIV D13
        else
          READ_CRF_ITEM := STORE [ CRFA DIV 2 ] MOD D13 ;
        CRFA := CRFA + 1
      end (* READ_CRF_ITEM *) ;


   begin (* PROGRAM_LOADER *)
     RLIB := ( KLIE - RLSC - KLSC ) DIV 32 * 32 ;

     (***********************************)
     (*increment entries in future list:*)
     (***********************************)

     for I := 0 to FLSC - 1 do
       STORE [ FLIB + I ] := STORE [ FLIB + I ] + RLIB ;

     (*****************************)
     (*move KLI to final position:*)
     (*****************************)

     for I := KLSC - 1 DOWNTO 0 do
       STORE [ RLIB + RLSC + I ] := STORE [ KLIB + I ] ;
     KLIB := RLIB + RLSC ;

     (****************************)
     (*prepare mcp-need analysis:*)
     (****************************)

     MCPE := RLIB ;
     MCP_COUNT := 0 ;
     for I := 0 to 127 do
       STORE [ MLIB + I ] := 0 ;

     (*************************************************)
     (*determine primary need of MCP's from name list:*)
     (*************************************************)

     I := NLSC0 ;
     while I > NLSCOP do
       begin
         ID := STORE [ NLIB + I - 1 ] ;
         if STORE [ NLIB + I - 2 ] MOD D3 = 0 then

     (***********************************)
     (*at most 4 letter/digit identifier*)
     (***********************************)

           I := I - 2
         else  (*at least 5 letters or digits*)
           I := I - 3 ;
         if ( ID DIV D15 ) MOD 2 = 0 then
           begin  (*MCP is used*)
             MCP_COUNT := MCP_COUNT + 1 ;
             STORE [ MLIB + ( STORE [ FLIB + ID MOD D15 ] - RLIB ) MOD
                              D15 ] := - ( FLIB + ID MOD D15 )
           end (* then *)
       end (* while *) ;

     (**********************************************************)
     (*determine secondary need using the cross-reference list:*)
     (**********************************************************)

     CRFA := 2 * CRFB ;
     LL := READ_CRF_ITEM  (*for MCP length*) ;
     while LL <> 7680  (*end marker*) do
       begin
         I := READ_CRF_ITEM  (*for MCP number*) ;
         USE := ( STORE [ MLIB + I ] <> 0 ) ;
         J := READ_CRF_ITEM

     (*******************************************)
     (*for number of MCP needing the current one*)
     (*******************************************)

              ;
         while J <> 7680  (*end marker*) do
           begin
             USE := USE or ( STORE [ MLIB + J ] <> 0 ) ;
             J := READ_CRF_ITEM
           end (* while *) ;
         if USE then
           begin
             MCPE := MCPE - LL ;
             if MCPE <= MCPB then
               STOP ( 25 ) ;
             if STORE [ MLIB + I ] < 0 then  (*primary need*)
               STORE [ - STORE [ MLIB + I ] ] := MCPE
             else  (*only secondary need*)
               MCP_COUNT := MCP_COUNT + 1 ;
             STORE [ MLIB + I ] := MCPE
           end (* then *) ;
         LL := READ_CRF_ITEM
       end (* while *) ;

     (***********************)
     (*load result list RLI:*)
     (***********************)

     LL := RLSC ;
     READ_LOCATION := RNSB ;
     PREP_READ_BITSTR1 ;
     LIST_ADDRESS := RLIB ;
     READ_LIST ;
     if STORE [ RLIB ] <> OPC_TABLE [ 89  (*START*) ] then
       STOP ( 101 ) ;
     TYP_ADDRESS ( RLIB ) ;

     (***********)
     (*copy MLI:*)
     (***********)

     for I := 0 to 127 do
       STORE [ CRFB + I ] := STORE [ MLIB + I ] ;
     KLIB := CRFB ;
     FLSC := 0 ;

     (************************)
     (*load MCP's from store:*)
     (************************)

     PREP_READ_BITSTR2 ;
     LL := READ_BIT_STRING ( 13 )  (*for length or end marker*) ;
     while LL < 7680 do
       begin
         I := READ_BIT_STRING ( 13 )  (*for MCP number*) ;
         LIST_ADDRESS := STORE [ CRFB + I ] ;
         if LIST_ADDRESS <> 0 then
           begin
             READ_LIST ;
             TEST_BIT_STOCK ;
             MCP_COUNT := MCP_COUNT - 1 ;
             STORE [ CRFB + I ] := 0
           end (* then *)
         else
           repeat
             READ_LOCATION := READ_LOCATION - 1
           until STORE [ READ_LOCATION ] = 63 * D21 ;
         PREP_READ_BITSTR2 ;
         LL := READ_BIT_STRING ( 13 )
       end (* while *) ;

     (***********************)
     (*load MCP's from tape:*)
     (***********************)

     RESET ( LIB_TAPE ) ;
     while MCP_COUNT <> 0 do
       begin
         WRITELN ( OUTPUT ) ;
         WRITELN ( OUTPUT ,
                   'load (next) library tape into the tape reader' ) ;
         PREP_READ_BITSTR3 ;
         LL := READ_BIT_STRING ( 13 )  (*for length or end marker*) ;
         while LL < 7680 do
           begin
             I := READ_BIT_STRING ( 13 )  (*for MCP number*) ;
             LIST_ADDRESS := STORE [ CRFB + I ] ;
             if LIST_ADDRESS <> 0 then
               begin
                 READ_LIST ;
                 TEST_BIT_STOCK ;
                 MCP_COUNT := MCP_COUNT - 1 ;
                 STORE [ CRFB + I ] := 0
               end (* then *)
             else
               repeat
                 repeat
                   READ ( LIB_TAPE , LL )
                 until LL = 0 ;
                 READ ( LIB_TAPE , LL )
               until LL = 0 ;
             PREP_READ_BITSTR3 ;
             LL := READ_BIT_STRING ( 13 )
           end (* while *)
       end (* while *) ;

     (****************************)
     (*program loading completed:*)
     (****************************)

     TYP_ADDRESS ( MCPE )
   end (* PROGRAM_LOADER *) ;





(**************)
(*main program*)
(**************)




begin (* HAUPTPROGRAMM *)

  (****)
  (*HT*)
  (****)

  WORD_DEL_TABLE [ 10 ] := 15086 ;
  WORD_DEL_TABLE [ 11 ] := 43 ;
  WORD_DEL_TABLE [ 12 ] := 1 ;
  WORD_DEL_TABLE [ 13 ] := 86 ;
  WORD_DEL_TABLE [ 14 ] := 13353 ;
  WORD_DEL_TABLE [ 15 ] := 10517 ;
  WORD_DEL_TABLE [ 16 ] := 81 ;
  WORD_DEL_TABLE [ 17 ] := 10624 ;
  WORD_DEL_TABLE [ 18 ] := 44 ;
  WORD_DEL_TABLE [ 19 ] := 0 ;
  WORD_DEL_TABLE [ 20 ] := 0 ;
  WORD_DEL_TABLE [ 21 ] := 10866 ;
  WORD_DEL_TABLE [ 22 ] := 0 ;
  WORD_DEL_TABLE [ 23 ] := 0 ;
  WORD_DEL_TABLE [ 24 ] := 106 ;
  WORD_DEL_TABLE [ 25 ] := 112 ;
  WORD_DEL_TABLE [ 26 ] := 0 ;
  WORD_DEL_TABLE [ 27 ] := 14957 ;
  WORD_DEL_TABLE [ 28 ] := 2 ;
  WORD_DEL_TABLE [ 29 ] := 2 ;
  WORD_DEL_TABLE [ 30 ] := 95 ;
  WORD_DEL_TABLE [ 31 ] := 115 ;
  WORD_DEL_TABLE [ 32 ] := 14304 ;
  WORD_DEL_TABLE [ 33 ] := 0 ;
  WORD_DEL_TABLE [ 34 ] := 0 ;
  WORD_DEL_TABLE [ 35 ] := 0 ;
  WORD_DEL_TABLE [ 36 ] := 0 ;
  WORD_DEL_TABLE [ 37 ] := 0 ;
  WORD_DEL_TABLE [ 38 ] := 107 ;

  (******************************)
  (*initialization of flex_table*)
  (*LK                          *)
  (******************************)

  FLEX_TABLE [ 0 ] := - 2 ;
  FLEX_TABLE [ 1 ] := 19969 ;
  FLEX_TABLE [ 2 ] := 16898 ;
  FLEX_TABLE [ 3 ] := - 0 ;
  FLEX_TABLE [ 4 ] := 18436 ;
  FLEX_TABLE [ 5 ] := - 0 ;
  FLEX_TABLE [ 6 ] := - 0 ;
  FLEX_TABLE [ 7 ] := 25863 ;
  FLEX_TABLE [ 8 ] := 25096 ;
  FLEX_TABLE [ 9 ] := - 0 ;
  FLEX_TABLE [ 10 ] := - 0 ;
  FLEX_TABLE [ 11 ] := - 1 ;
  FLEX_TABLE [ 12 ] := - 0 ;
  FLEX_TABLE [ 13 ] := - 1 ;
  FLEX_TABLE [ 14 ] := 41635 ;
  FLEX_TABLE [ 15 ] := - 0 ;
  FLEX_TABLE [ 16 ] := 31611 ;
  FLEX_TABLE [ 17 ] := - 0 ;
  FLEX_TABLE [ 18 ] := - 0 ;
  FLEX_TABLE [ 19 ] := 17155 ;
  FLEX_TABLE [ 20 ] := - 0 ;
  FLEX_TABLE [ 21 ] := 23301 ;
  FLEX_TABLE [ 22 ] := 25606 ;
  FLEX_TABLE [ 23 ] := - 0 ;
  FLEX_TABLE [ 24 ] := - 0 ;
  FLEX_TABLE [ 25 ] := 25353 ;
  FLEX_TABLE [ 26 ] := 30583 ;
  FLEX_TABLE [ 27 ] := - 0 ;
  FLEX_TABLE [ 28 ] := - 1 ;
  FLEX_TABLE [ 29 ] := - 0 ;
  FLEX_TABLE [ 30 ] := - 0 ;
  FLEX_TABLE [ 31 ] := - 1 ;
  FLEX_TABLE [ 32 ] := 19712 ;
  FLEX_TABLE [ 33 ] := - 0 ;
  FLEX_TABLE [ 34 ] := - 0 ;
  FLEX_TABLE [ 35 ] := 14365 ;
  FLEX_TABLE [ 36 ] := - 0 ;
  FLEX_TABLE [ 37 ] := 14879 ;
  FLEX_TABLE [ 38 ] := 15136 ;
  FLEX_TABLE [ 39 ] := - 0 ;
  FLEX_TABLE [ 40 ] := - 0 ;
  FLEX_TABLE [ 41 ] := 15907 ;
  FLEX_TABLE [ 42 ] := - 1 ;
  FLEX_TABLE [ 43 ] := - 0 ;
  FLEX_TABLE [ 44 ] := - 1 ;
  FLEX_TABLE [ 45 ] := - 0 ;
  FLEX_TABLE [ 46 ] := - 0 ;
  FLEX_TABLE [ 47 ] := - 1 ;
  FLEX_TABLE [ 48 ] := - 0 ;
  FLEX_TABLE [ 49 ] := 17994 ;
  FLEX_TABLE [ 50 ] := 14108 ;
  FLEX_TABLE [ 51 ] := - 0 ;
  FLEX_TABLE [ 52 ] := 14622 ;
  FLEX_TABLE [ 53 ] := - 0 ;
  FLEX_TABLE [ 54 ] := - 0 ;
  FLEX_TABLE [ 55 ] := 15393 ;
  FLEX_TABLE [ 56 ] := 15650 ;
  FLEX_TABLE [ 57 ] := - 0 ;
  FLEX_TABLE [ 58 ] := - 0 ;
  FLEX_TABLE [ 59 ] := 30809 ;
  FLEX_TABLE [ 60 ] := - 0 ;
  FLEX_TABLE [ 61 ] := - 1 ;
  FLEX_TABLE [ 62 ] := 30326 ;
  FLEX_TABLE [ 63 ] := - 0 ;
  FLEX_TABLE [ 64 ] := 19521 ;
  FLEX_TABLE [ 65 ] := - 0 ;
  FLEX_TABLE [ 66 ] := - 0 ;
  FLEX_TABLE [ 67 ] := 12309 ;
  FLEX_TABLE [ 68 ] := - 0 ;
  FLEX_TABLE [ 69 ] := 12823 ;
  FLEX_TABLE [ 70 ] := 13080 ;
  FLEX_TABLE [ 71 ] := - 0 ;
  FLEX_TABLE [ 72 ] := - 0 ;
  FLEX_TABLE [ 73 ] := 13851 ;
  FLEX_TABLE [ 74 ] := - 1 ;
  FLEX_TABLE [ 75 ] := - 0 ;
  FLEX_TABLE [ 76 ] := - 1 ;
  FLEX_TABLE [ 77 ] := - 0 ;
  FLEX_TABLE [ 78 ] := - 0 ;
  FLEX_TABLE [ 79 ] := - 1 ;
  FLEX_TABLE [ 80 ] := - 0 ;
  FLEX_TABLE [ 81 ] := 11795 ;
  FLEX_TABLE [ 82 ] := 12052 ;
  FLEX_TABLE [ 83 ] := - 0 ;
  FLEX_TABLE [ 84 ] := 12566 ;
  FLEX_TABLE [ 85 ] := - 0 ;
  FLEX_TABLE [ 86 ] := - 0 ;
  FLEX_TABLE [ 87 ] := 13337 ;
  FLEX_TABLE [ 88 ] := 13594 ;
  FLEX_TABLE [ 89 ] := - 0 ;
  FLEX_TABLE [ 90 ] := - 0 ;
  FLEX_TABLE [ 91 ] := 31319 ;
  FLEX_TABLE [ 92 ] := - 0 ;
  FLEX_TABLE [ 93 ] := - 1 ;
  FLEX_TABLE [ 94 ] := - 1 ;
  FLEX_TABLE [ 95 ] := - 0 ;
  FLEX_TABLE [ 96 ] := - 0 ;
  FLEX_TABLE [ 97 ] := 9482 ;
  FLEX_TABLE [ 98 ] := 9739 ;
  FLEX_TABLE [ 99 ] := - 0 ;
  FLEX_TABLE [ 100 ] := 10253 ;
  FLEX_TABLE [ 101 ] := - 0 ;
  FLEX_TABLE [ 102 ] := - 0 ;
  FLEX_TABLE [ 103 ] := 11024 ;
  FLEX_TABLE [ 104 ] := 11281 ;
  FLEX_TABLE [ 105 ] := - 0 ;
  FLEX_TABLE [ 106 ] := - 0 ;
  FLEX_TABLE [ 107 ] := 31832 ;
  FLEX_TABLE [ 108 ] := - 0 ;
  FLEX_TABLE [ 109 ] := - 1 ;
  FLEX_TABLE [ 110 ] := - 1 ;
  FLEX_TABLE [ 111 ] := - 0 ;
  FLEX_TABLE [ 112 ] := 31040 ;
  FLEX_TABLE [ 113 ] := - 0 ;
  FLEX_TABLE [ 114 ] := - 0 ;
  FLEX_TABLE [ 115 ] := 9996 ;
  FLEX_TABLE [ 116 ] := - 0 ;
  FLEX_TABLE [ 117 ] := 10510 ;
  FLEX_TABLE [ 118 ] := 10767 ;
  FLEX_TABLE [ 119 ] := - 0 ;
  FLEX_TABLE [ 120 ] := - 0 ;
  FLEX_TABLE [ 121 ] := 11538 ;
  FLEX_TABLE [ 122 ] := - 2 ;
  FLEX_TABLE [ 123 ] := - 0 ;
  FLEX_TABLE [ 124 ] := - 2 ;
  FLEX_TABLE [ 125 ] := - 0 ;
  FLEX_TABLE [ 126 ] := - 0 ;
  FLEX_TABLE [ 127 ] := - 2 ;

  (************************)
  (*preparation of prescan*)
  (*LE                    *)
  (************************)

  RNS_STATE := VIRGINAL ;
  SCAN := 1 ;
  READ_UNTIL_NEXT_DELImiter ;
  PRESCAN ;

  (***********************************************************)
  (*HK                                                       *)
  (*writeln;                                                 *)
  (*     for bn:= plib to plie do writeln(bn:5,store[bn]:10);*)
  (*     writeln;                                            *)
  (***********************************************************)


  (***************************)
  (*preparation of main scan:*)
  (*HL                       *)
  (***************************)

  RNS_STATE := VIRGINAL ;
  SCAN := - 1 ;
  IFLAG := 0 ;
  MFLAG := 0 ;
  VFLAG := 0 ;
  BN := 0 ;
  AFLAG := 0 ;
  SFLAG := 0 ;
  EFLAG := 0 ;
  RLSC := 0 ;
  FLSC := 0 ;
  KLSC := 0 ;
  VLAM := 0 ;
  FLIB := RNSB + 1 ;
  KLIB := FLIB + 16 ;
  NLIB := KLIB + 16 ;
  if NLIB + NLSC0 >= PLIB then
    STOP ( 25 ) ;
  NLSC := NLSC0 ;
  TLSC := TLIB ;
  GVC := GVC0 ;
  FILL_T_LIST ( 161 ) ;

  (***********************)
  (*prefill of name list:*)
  (***********************)

  STORE [ NLIB + 0 ] := 27598040 ;
  STORE [ NLIB + 1 ] := 265358 ;              (*read*)
  STORE [ NLIB + 2 ] := 134217727 - 6 ;
  STORE [ NLIB + 3 ] := 61580507 ;
  STORE [ NLIB + 4 ] := 265359 ;              (*print*)
  STORE [ NLIB + 5 ] := 134217727 - 53284863 ;
  STORE [ NLIB + 6 ] := 265360 ;              (*TAB*)
  STORE [ NLIB + 7 ] := 134217727 - 19668591 ;
  STORE [ NLIB + 8 ] := 265361 ;              (*NLCR*)
  STORE [ NLIB + 9 ] := 134217727 - 0 ;
  STORE [ NLIB + 10 ] := 134217727 - 46937177 ;
  STORE [ NLIB + 11 ] := 265363 ;             (*SPACE*)
  STORE [ NLIB + 12 ] := 53230304 ;
  STORE [ NLIB + 13 ] := 265364 ;             (*stop*)
  STORE [ NLIB + 14 ] := 59085824 ;
  STORE [ NLIB + 15 ] := 265349 ;             (*abs*)
  STORE [ NLIB + 16 ] := 48768224 ;
  STORE [ NLIB + 17 ] := 265350 ;             (*sign*)
  STORE [ NLIB + 18 ] := 61715680 ;
  STORE [ NLIB + 19 ] := 265351 ;             (*sqrt*)
  STORE [ NLIB + 20 ] := 48838656 ;
  STORE [ NLIB + 21 ] := 265352 ;             (*sin*)
  STORE [ NLIB + 22 ] := 59512832 ;
  STORE [ NLIB + 23 ] := 265353 ;             (*cos*)
  STORE [ NLIB + 24 ] := 48922624 ;
  STORE [ NLIB + 25 ] := 265355 ;             (*ln*)
  STORE [ NLIB + 26 ] := 53517312 ;
  STORE [ NLIB + 27 ] := 265356 ;             (*exp*)
  STORE [ NLIB + 28 ] := 134217727 - 289 ;
  STORE [ NLIB + 29 ] := 29964985 ;
  STORE [ NLIB + 30 ] := 265357 ;             (*entier*)
  STORE [ NLIB + 31 ] := 134217727 - 29561343 ;
  STORE [ NLIB + 32 ] := 294912 ;             (*SUM*)
  STORE [ NLIB + 33 ] := 134217727 - 14789691 ;
  STORE [ NLIB + 34 ] := 134217727 - 15115337 ;
  STORE [ NLIB + 35 ] := 294913 ;             (*PRINTTEXT*)
  STORE [ NLIB + 36 ] := 134217727 - 27986615 ;
  STORE [ NLIB + 37 ] := 294914 ;             (*EVEN*)
  STORE [ NLIB + 38 ] := 134217727 - 325 ;
  STORE [ NLIB + 39 ] := 21928153 ;
  STORE [ NLIB + 40 ] := 294915 ;             (*arctan*)
  STORE [ NLIB + 41 ] := 134217727 - 15081135 ;
  STORE [ NLIB + 42 ] := 294917 ;             (*FLOT*)
  STORE [ NLIB + 43 ] := 134217727 - 14787759 ;
  STORE [ NLIB + 44 ] := 294918 ;             (*FIXT*)
  STORE [ NLIB + 45 ] := 134217727 - 3610 ;
  STORE [ NLIB + 46 ] := 134217727 - 38441163 ;
  STORE [ NLIB + 47 ] := 294936 ;             (*ABSFIXT*)
  INTRO_NEW_BLOCK2 ;
  BITCOUNT := 0 ;
  BITSTOCK := 0 ;
  RNSB := BIM ;
  FILL_RESULT_LIST ( 96  (*START*) , 0 ) ;
  POS := 0 ;
  MAIN_SCAN ;                                                         (*EL*)
  FILL_RESULT_LIST ( 97  (*STOP*) , 0 ) ;

  (***************************************)
  (*writeln; writeln('FLI:');            *)
  (*     for bn:= 0 to flsc-1 do         *)
  (*     writeln(bn:5,store[flib+bn]:10);*)
  (***************************************)


  (***************************************************************************)
  (*writeln; writeln('KLI:');                                                *)
  (*     for bn:= 0 to klsc-1 do                                             *)
  (*     writeln(bn:5,store[klib+bn]:10,                                     *)
  (*            (store[klib+bn] mod 134217728) div 16777216 : 10,            *)
  (*                        (store[klib+bn] mod  16777216) div  2097152 : 2, *)
  (*                        (store[klib+bn] mod   2097152) div   524288 : 3, *)
  (*                        (store[klib+bn] mod    524288) div   131072 : 2, *)
  (*                        (store[klib+bn] mod    131072) div    32768 : 2, *)
  (*                        (store[klib+bn] mod     32768) div     1024 : 4, *)
  (*                        (store[klib+bn] mod      1024) div       32 : 3, *)
  (*                        (store[klib+bn] mod        32) div        1 : 3);*)
  (***************************************************************************)


  (*******************************)
  (*preparation of program loader*)
  (*******************************)

  OPC_TABLE [ 0 ] := 33 ;
  OPC_TABLE [ 1 ] := 34 ;
  OPC_TABLE [ 2 ] := 16 ;
  OPC_TABLE [ 3 ] := 56 ;
  OPC_TABLE [ 4 ] := 58 ;
  OPC_TABLE [ 5 ] := 85 ;
  OPC_TABLE [ 6 ] := 9 ;
  OPC_TABLE [ 7 ] := 14 ;
  OPC_TABLE [ 8 ] := 18 ;
  OPC_TABLE [ 9 ] := 30 ;
  OPC_TABLE [ 10 ] := 13 ;
  OPC_TABLE [ 11 ] := 17 ;
  OPC_TABLE [ 12 ] := 19 ;
  OPC_TABLE [ 13 ] := 20 ;
  OPC_TABLE [ 14 ] := 31 ;
  OPC_TABLE [ 15 ] := 35 ;
  OPC_TABLE [ 16 ] := 39 ;
  OPC_TABLE [ 17 ] := 61 ;
  OPC_TABLE [ 18 ] := 8 ;
  OPC_TABLE [ 19 ] := 10 ;
  OPC_TABLE [ 20 ] := 11 ;
  OPC_TABLE [ 21 ] := 12 ;
  OPC_TABLE [ 22 ] := 15 ;
  for II := 23 to 31 do
    OPC_TABLE [ II ] := II - 2 ;
  OPC_TABLE [ 32 ] := 32 ;
  OPC_TABLE [ 33 ] := 36 ;
  OPC_TABLE [ 34 ] := 37 ;
  OPC_TABLE [ 35 ] := 38 ;
  for II := 36 to 51 do
    OPC_TABLE [ II ] := II + 4 ;
  OPC_TABLE [ 52 ] := 57 ;
  OPC_TABLE [ 53 ] := 59 ;
  OPC_TABLE [ 54 ] := 60 ;
  for II := 55 to 102 do
    OPC_TABLE [ II ] := II + 7 ;
  STORE [ CRFB + 0 ] := 30 * D13 + 0 ;
  STORE [ CRFB + 1 ] := 7680 * D13 + 20 ;
  STORE [ CRFB + 2 ] := 1 * D13 + 7680 ;
  STORE [ CRFB + 3 ] := 12 * D13 + 2 ;
  STORE [ CRFB + 4 ] := 7680 * D13 + 63 ;
  STORE [ CRFB + 5 ] := 3 * D13 + 7680 ;
  STORE [ CRFB + 6 ] := 15 * D13 + 4 ;
  STORE [ CRFB + 7 ] := 3 * D13 + 7680 ;
  STORE [ CRFB + 8 ] := 100 * D13 + 5 ;
  STORE [ CRFB + 9 ] := 7680 * D13 + 134 ;
  STORE [ CRFB + 10 ] := 6 * D13 + 24 ;
  STORE [ CRFB + 11 ] := 7680 * D13 + 21 ;
  STORE [ CRFB + 12 ] := 24 * D13 + 7680 ;
  STORE [ CRFB + 13 ] := 7680 * D13 + 7680 ;
  STORE [ MCPB ] := 63 * D21 ;
  STORE [ MCPB + 1 ] := 63 * D21 ;
  PROGRAM_LOADER ;
  WRITELN ( OUTPUT ) ;
  WRITELN ( OUTPUT ) ;
  WRITELN ( OUTPUT ) ;
  for II := MCPE to RLIB + RLSC + KLSC - 1 do
    WRITELN ( OUTPUT , II : 5 , STORE [ II ] : 9 )
end (* HAUPTPROGRAMM *) .
