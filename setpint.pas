program TEST ;


begin (* HAUPTPROGRAMM *)
  case X of
    PINT : begin
             if L . DTYPE <> PSET then
               ERROR ( 615 ) ;
             if R . DTYPE <> PSET then
               ERROR ( 615 ) ;
             STKADR := L . STKADR ;

  (****************************************)
  (* len = minimum length of the operands *)
  (* ok on intersection                   *)
  (****************************************)

             LEN := L . PLEN ;
             if LEN > R . PLEN then
               LEN := R . PLEN ;

  (********************************************)
  (* ONE time loop - using break to terminate *)
  (********************************************)

             repeat

  (*****************************)
  (* ONE OR BOTH OPERANDS NULL *)
  (*****************************)

               if LEN <= 0 then
                 begin
                   if R . PLEN <= 0 then
                     begin
                       FREEREG ( L ) ;
                       L := R
                     end (* then *)
                   else
                     FREEREG ( R ) ;
                   break ;
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
                     L . PCNST -> . S [ I ] := L . PCNST -> . S [ I ] *
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
                   if L . PLEN > R . PLEN then
                     begin
                       LOAD ( R ) ;
                       LR := FALSE
                     end (* then *)
                   else
                     if L . PLEN < R . PLEN then
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

                         GENRR ( XNR , L . RGADR , R . RGADR ) ;
                         AVAIL [ R . RGADR ] := TRUE ;
                         if L . PLEN > 4 then
                           GENRR ( XNR , L . RGADR + 1 , R . RGADR + 1
                                   ) ;
                         if R . PLEN > 4 then
                           AVAIL [ R . RGADR + 1 ] := TRUE ;
                       end (* then *)
                     else

  (*******************************************)
  (* LEFT OPND IN REGS, RIGHT OPND IN MEMORY *)
  (*******************************************)

                       begin
                         GETOPERAND ( R , Q2 , P2 , B2 ) ;
                         GENRX ( XN , L . RGADR , Q2 , P2 , B2 ) ;
                         if L . PLEN > 4 then
                           begin
                             CHECKDISP ( Q2 , P2 , B2 ) ;
                             GENRX ( XN , L . RGADR + 1 , Q2 + 4 , P2 ,
                                     B2 )
                           end (* then *)
                       end (* else *)
                   else

  (******************************************)
  (* LEFT OPND IN REGS, RIGHT OPND IS CONST *)
  (******************************************)

                     begin
                       I_S_R . S := R . PCNST -> . S [ 1 ] ;
                       if I_S_R . I1 <> - 1 then
                         if I_S_R . I1 <> 0 then
                           GENRXLIT ( XN , L . RGADR , I_S_R . I1 , 0 )
                         else
                           GENRR ( XSR , L . RGADR , L . RGADR ) ;
                       if LEN > 4 then
                         GENRXLIT ( XN , L . RGADR + 1 , I_S_R . I2 , 0
                                    )
                       else
                         if L . PLEN > 4 then
                           AVAIL [ L . RGADR + 1 ] := TRUE ;
                     end (* else *) ;
                   break ;
                 end (* then *) ;

  (*****************************************)
  (* len > 8 - most complicated situation  *)
  (*****************************************)

               FORCESTK ( L ) ;
               if R . VRBL then
                 begin

  (***************************)
  (* BOTH OPERANDS IN MEMORY *)
  (***************************)

                   GETQB ( L , Q1 , P1 , 0 ) ;
                   TXRG := TRG1 ;
                   GETQB ( R , Q2 , P2 , 0 ) ;
                   TXRG := TRG14 ;
                   GENSS ( XNC , LEN , Q1 , P1 , Q2 , P2 ) ;
                 end (* then *)
               else
                 begin

  (*****************************************)
  (* LEFT OPND IN MEM, RIGHT OPND IS CONST *)
  (*****************************************)

                   COMPACT ( R . PCNST -> , LEN , J , CHR ( 255 ) ) ;
                   GETQB ( L , Q1 , P1 , J ) ;
                   LEN := ALIGN ( LEN , INTSIZE ) ;
                   if LEN >= J then
                     GENSSLIT ( XNC , LEN - J , Q1 + J , P1 , R . PCNST
                                -> ) ;
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
