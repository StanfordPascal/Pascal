program TRYIT ( OUTPUT ) ;


const CMAX = 100000 ;
      LMAX = 100 ;
      OMAX = 63 ;
      MNMAX = - 1073741825 ;     (* works  = (maxint / 2) *)
      NMAX = 1073741825 ;        (* works  = (maxint / 2) *)
                                 (* nmax  = 1073741825;   *)
                                 (* doesn't work          *)
                                 (* nmax = maxint;        *)
                                 (* doesn't work          *)


type ORDER = packed record
                      F : - OMAX .. + OMAX ;
                      X : - LMAX .. + LMAX ;
                      Y : - NMAX .. + NMAX ;
                    end ;


var CODE : array [ 0 .. CMAX ] of ORDER ;


begin (* HAUPTPROGRAMM *)
  if MNMAX > NMAX then
    WRITELN ( 'error' ) ;
  CODE [ 0 ] . F := 1 ;
  CODE [ 0 ] . X := 2 ;
  WRITELN ( 'tryit' )
end (* HAUPTPROGRAMM *) .
