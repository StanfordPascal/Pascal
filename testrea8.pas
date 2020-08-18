program TESTREA8 ( INPUT , OUTPUT ) ;


var I : INTEGER ;
    ANZ : INTEGER ;


begin (* HAUPTPROGRAMM *)
  ANZ := 0 ;
  READ ( I ) ;
  while not EOF ( INPUT ) do
    begin
      WRITE ( ' ' , I : 8 ) ;
      ANZ := ANZ + 1 ;
      if ANZ MOD 8 = 0 then
        WRITELN ;
      READ ( I ) ;
    end (* while *)
end (* HAUPTPROGRAMM *) .
