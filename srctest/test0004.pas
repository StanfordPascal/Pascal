program ROMAN ( OUTPUT ) ;

//***********************
// write roman numerals
//***********************



var X , Y : INTEGER ;


begin (* HAUPTPROGRAMM *)
  Y := 1 ;
  repeat
    X := Y ;
    WRITE ( X , ' ' ) ;
    while X >= 1000 do
      begin
        WRITE ( 'm' ) ;
        X := X - 1000
      end (* while *) ;
    if X >= 500 then
      begin
        WRITE ( 'd' ) ;
        X := X - 500
      end (* then *) ;
    while X >= 100 do
      begin
        WRITE ( 'c' ) ;
        X := X - 100
      end (* while *) ;
    if X >= 50 then
      begin
        WRITE ( 'l' ) ;
        X := X - 50
      end (* then *) ;
    while X >= 10 do
      begin
        WRITE ( 'x' ) ;
        X := X - 10
      end (* while *) ;
    if X >= 5 then
      begin
        WRITE ( 'v' ) ;
        X := X - 5
      end (* then *) ;
    while X >= 1 do
      begin
        WRITE ( 'i' ) ;
        X := X - 1
      end (* while *) ;
    WRITELN ;
    Y := 2 * Y ;
  until Y > 5000
end (* HAUPTPROGRAMM *) .
