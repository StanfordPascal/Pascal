program TEST ;


begin (* HAUPTPROGRAMM *)
  case X of
    PUNI : begin
             if L . DTYPE <> PSET then
               ERROR ( 615 ) ;
             if R . DTYPE <> PSET then
               ERROR ( 615 ) ;
             STKADR := L . STKADR ;

  (****************************************)
  (* len = maximum length of the operands *)
  (* ok on union                          *)
  (****************************************)

             LEN := L . PLEN ;
             if LEN < R . PLEN then
               LEN := R . PLEN ;

  (********************************************)
  (* ONE time loop - using break to terminate *)
  (********************************************)

             repeat

  (******************************************************)
  (* the right operand is null                          *)
  (* nothing to do                                      *)
  (******************************************************)

               if R . PLEN <= 0 then
                 break ;

  (******************************************************)
  (* the left operand is null                           *)
  (* replace the left operand with the right operand    *)
  (******************************************************)

               if L . PLEN <= 0 then
                 begin
                   if ( R . STKADR <> STKADR ) and R . VRBL and R .
                   DRCT and ( R . VPA = ONSTK ) then
                     begin
                       L . VRBL := TRUE ;
                       L . DRCT := TRUE ;
                       L . VPA := ONSTK ;
                       GETQB ( L , Q1 , P1 , 0 ) ;
                       TXRG := TRG1 ;
                       GETQB ( R , Q2 , P2 , 0 ) ;
                       TXRG := TRG14 ;
                       GENSS ( XMVC , LEN , Q1 , P1 , Q2 , P2 ) ;
                     end (* then *)
                   else
                     L := R ;
                   break
                 end (* then *) ;

  (******************************)
  (* BOTH OPERANDS ARE NOT NULL *)
  (******************************)

               if not L . VRBL and not R . VRBL then

  (**********************************************************)
  (* both operands are constants, operation at compile time *)
  (**********************************************************)

                 begin
                   for I := 1 to MXSETINX do
                     L . PCNST -> . S [ I ] := L . PCNST -> . S [ I ] +
                                               R . PCNST -> . S [ I ] ;
                   MINCONSTSET ;
                   break ;
                 end (* then *) ;

  (*****************************************)
  (* one of the operands is not a constant *)
  (*****************************************)

               if LEN <= 8 then

  (*******************************************)
  (* len <= 8 - generate result in registers *)
  (*******************************************)

                 begin
                   LR := TRUE ;
                   if L . PLEN < R . PLEN then
                     begin
                       LOAD ( R ) ;
                       LR := FALSE
                     end (* then *)
                   else
                     if L . PLEN > R . PLEN then
                       LOAD ( L )
                     else

  (*************************)
  (* EQUAL LENGTH OPERANDS *)
  (*************************)

                       if not ( L . VRBL and L . DRCT and ( L . VPA =
                       RGS ) ) then
                         if R . VRBL and R . DRCT and ( R . VPA = RGS )
                         then
                           LR := FALSE
                         else
                           LOAD ( L ) ;
                   if not LR then

  (************************)
  (* INTERCHANGE OPERANDS *)
  (************************)

                     begin
                       L := R ;
                       R := STK [ TOP - 1 ]
                     end (* then *) ;
                   if R . VRBL then
                     if R . DRCT and ( R . VPA = RGS ) then
                       begin

  (******************************)
  (* BOTH OPERANDS IN REGISTERS *)
  (******************************)

                         GENRR ( XORX , L . RGADR , R . RGADR ) ;
                         AVAIL [ R . RGADR ] := TRUE ;
                         if R . PLEN > 4 then
                           begin
                             GENRR ( XORX , L . RGADR + 1 , R . RGADR +
                                     1 ) ;
                             AVAIL [ R . RGADR + 1 ] := TRUE
                           end (* then *)
                       end (* then *)
                     else

  (*******************************************)
  (* LEFT OPND IN REGS, RIGHT OPND IN MEMORY *)
  (*******************************************)

                       begin
                         GETOPERAND ( R , Q2 , P2 , B2 ) ;
                         GENRX ( XO , L . RGADR , Q2 , P2 , B2 ) ;
                         if R . PLEN > 4 then
                           begin
                             CHECKDISP ( Q2 , P2 , B2 ) ;
                             GENRX ( XO , L . RGADR + 1 , Q2 + 4 , P2 ,
                                     B2 ) ;
                           end (* then *)
                       end (* else *)
                   else

  (******************************************)
  (* LEFT OPND IN REGS, RIGHT OPND IS CONST *)
  (******************************************)

                     begin
                       I_S_R . S := R . PCNST -> . S [ 1 ] ;
                       if I_S_R . I1 <> 0 then
                         GENRXLIT ( XO , L . RGADR , I_S_R . I1 , 0 ) ;
                       if R . PLEN > 4 then
                         if I_S_R . I2 <> 0 then
                           GENRXLIT ( XO , L . RGADR + 1 , I_S_R . I2 ,
                                      0 )
                     end (* else *) ;
                   break ;
                 end (* then *) ;

  (*****************************************)
  (* len > 8 - most complicated situation  *)
  (*****************************************)

               FORCESTK ( L ) ;
               if R . VRBL then
                 if R . DRCT and ( R . VPA = RGS ) then
                   begin
                     GETQB ( L , Q1 , P1 , 4 ) ;
                     GENRX ( XO , R . RGADR , Q1 , P1 , 0 ) ;
                     AVAIL [ R . RGADR ] := TRUE ;
                     if R . PLEN > 4 then
                       begin
                         GENRX ( XO , R . RGADR + 1 , Q1 + 4 , P1 , 0 )
                                 ;
                         GENRS ( XSTM , R . RGADR , R . RGADR + 1 , Q1
                                 , P1 ) ;
                         AVAIL [ R . RGADR + 1 ] := TRUE
                       end (* then *)
                     else
                       GENRX ( XST , R . RGADR , Q1 , P1 , 0 )
                   end (* then *)
                 else

  (***************************)
  (* BOTH OPERANDS IN MEMORY *)
  (***************************)

                   begin
                     MIN := L . PLEN ;
                     if MIN > R . PLEN then
                       MIN := R . PLEN ;
                     GETQB ( L , Q1 , P1 , MIN ) ;
                     TXRG := TRG1 ;
                     GETQB ( R , Q2 , P2 , MIN ) ;
                     TXRG := TRG14 ;
                     GENSS ( XOC , MIN , Q1 , P1 , Q2 , P2 ) ;
                     if R . PLEN > L . PLEN then
                       GENSS ( XMVC , R . PLEN - L . PLEN , Q1 + MIN ,
                               P1 , Q2 + MIN , P2 )
                   end (* else *)
               else

  (*****************************************)
  (* LEFT OPND IN MEM, RIGHT OPND IS CONST *)
  (*****************************************)

                 begin
                   PSVAL . S := R . PCNST -> ;
                   MIN := L . PLEN ;
                   if MIN > R . PLEN then
                     MIN := R . PLEN ;
                   COMPACT ( R . PCNST -> , MIN , J , CHR ( 0 ) ) ;
                   GETQB ( L , Q1 , P1 , MIN ) ;
                   if MIN >= 0 then
                     GENSSLIT ( XOC , MIN , Q1 + J , P1 , R . PCNST ->
                                ) ;
                   if LEN > L . PLEN then
                     begin
                       for I := 1 to LEN - L . PLEN do
                         PSVAL . C [ I ] := PSVAL . C [ I + L . PLEN ]
                                            ;
                       GENSSLIT ( XMVC , LEN - L . PLEN , Q1 + R . PLEN
                                  , P1 , PSVAL . S ) ;
                     end (* then *)
                 end (* else *) ;
             until FALSE ;

  (*********************************************)
  (* this is done in any case before returning *)
  (*********************************************)

             L . STKADR := STKADR ;
             L . PLEN := LEN ;
           end (* tag/ca *) ;
  end (* case *)
end (* HAUPTPROGRAMM *) .
