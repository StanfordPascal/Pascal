program TEXTHEX ( OUTPUT ) ;


const SH = x'0d0a' 'zeile' x'0d0a' ;
      SB = b'00100001_00100001' ;
      SAPO = 'Hallo''Welt' ;


begin (* HAUPTPROGRAMM *)
  WRITELN ( SH ) ;
  WRITELN ( SB ) ;
  WRITELN ( SAPO ) ;
end (* HAUPTPROGRAMM *) .
