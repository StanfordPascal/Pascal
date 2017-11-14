program TEST ;


begin (* HAUPTPROGRAMM *)
  case X of
    PDIF : begin
             if L . DTYPE <> PSET then
               ERROR ( 615 ) ;
             if R . DTYPE <> PSET then
               ERROR ( 615 ) ;

  (****************************************)
  (* len = minimum length of the operands *)
  (* ok on set difference                 *)
  (****************************************)

             LEN := L . PLEN ;
             if LEN > R . PLEN then
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
  (******************************************************)

               if L . PLEN <= 0 then
                 begin
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
                   if LEN > R . PLEN then
                     for I := R . PLEN + 1 to LEN do
                       R . PCNST -> . C [ I ] := CHR ( 0 ) ;
                   for I := 1 to MXSETINX do
                     L . PCNST -> . S [ I ] := L . PCNST -> . S [ I ] -
                                               R . PCNST -> . S [ I ] ;
                   MINCONSTSET ;
                   break ;
                 end (* then *) ;

  (*****************************************)
  (* one of the operands is not a constant *)
  (*****************************************)

               if L . PLEN <= 8 then

  (*******************************************)
  (* len <= 8 - generate result in registers *)
  (*******************************************)

                 begin
                   LOAD ( L ) ;
                   if R . VRBL then
                     begin
                       if not ( R . DRCT and ( R . VPA = RGS ) ) then
                         begin

  (**************************)
  (* FORCE R INTO REGISTERS *)
  (**************************)

                           if R . PLEN > L . PLEN then
                             R . PLEN := L . PLEN ;
                           LOAD ( R ) ;
                         end (* then *) ;
                       GENRR ( XORX , L . RGADR , R . RGADR ) ;
                       GENRR ( XXR , L . RGADR , R . RGADR ) ;
                       AVAIL [ R . RGADR ] := TRUE ;
                       if R . PLEN > 4 then
                         begin
                           if L . PLEN > 4 then
                             begin
                               GENRR ( XORX , L . RGADR + 1 , R . RGADR
                                       + 1 ) ;
                               GENRR ( XXR , L . RGADR + 1 , R . RGADR
                                       + 1 ) ;
                             end (* then *) ;
                           AVAIL [ R . RGADR + 1 ] := TRUE ;
                         end (* then *)
                     end (* then *)
                   else
                     begin

  (*****************************************)
  (* LEFT OPND IN REGS, RIGHT OPND IS CNST *)
  (*****************************************)

                       I_S_R . S := [ 0 .. 63 ] - R . PCNST -> . S [ 1
                                    ] ;
                       if I_S_R . I1 <> - 1 then
                         if I_S_R . I1 <> 0 then
                           GENRXLIT ( XN , L . RGADR , I_S_R . I1 , 0 )
                         else
                           GENRR ( XSR , L . RGADR , L . RGADR ) ;
                       if ( L . PLEN > 4 ) and ( R . PLEN > 4 ) then
                         if I_S_R . I2 <> 0 then
                           GENRXLIT ( XN , L . RGADR + 1 , I_S_R . I2 ,
                                      0 )
                         else
                           begin
                             L . PLEN := 4 ;
                             AVAIL [ L . RGADR + 1 ] := TRUE
                           end (* else *) ;
                     end (* else *) ;
                   break ;
                 end (* then *) ;

  (*****************************************)
  (* len > 8 - most complicated situation  *)
  (*****************************************)

               FORCESTK ( L ) ;
               if R . VRBL then
                 begin
                   if not ( R . VRBL and R . DRCT and ( R . VPA = MEM )
                   ) then
                     FORCESTK ( R ) ;
                   GETQB ( L , Q1 , P1 , 0 ) ;
                   TXRG := TRG1 ;
                   GETQB ( R , Q2 , P2 , 0 ) ;
                   TXRG := TRG14 ;
                   GENSS ( XOC , LEN , Q1 , P1 , Q2 , P2 ) ;
                   GENSS ( XXC , LEN , Q1 , P1 , Q2 , P2 ) ;
                 end (* then *)
               else
                 begin

  (***********************)
  (* RIGHT OPND IS CONST *)
  (***********************)

                   for I := 1 to MXSETINX do
                     begin
                       R . PCNST -> . S [ I ] := [ 0 .. 63 ] - R .
                                                 PCNST -> . S [ I ] ;
                     end (* for *) ;
                   COMPACT ( R . PCNST -> , LEN , J , CHR ( 255 ) ) ;
                   GETQB ( L , Q1 , P1 , J ) ;
                   if LEN > 0 then
                     GENSSLIT ( XNC , LEN , Q1 + J , P1 , R . PCNST ->
                                ) ;
                 end (* else *)
             until FALSE
           end (* tag/ca *) ;
  end (* case *)
end (* HAUPTPROGRAMM *) .
