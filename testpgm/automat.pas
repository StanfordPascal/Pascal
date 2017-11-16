program AUTOMAT ( INPUT , OUTPUT ) ;

var EINGABEZEICHEN , FEHLERZEICHEN : CHAR ;
    ZUSTAND : INTEGER ;
    FEHLER : BOOLEAN ;

begin (* HAUPTPROGRAMM *)
  READ ( EINGABEZEICHEN ) ;
  ZUSTAND := 1 ;
  FEHLER := FALSE ;
  READ ( EINGABEZEICHEN ) ;
  while ( not EOF ( INPUT ) ) and ( not FEHLER ) do
    begin
      WRITE ( EINGABEZEICHEN ) ;
      case ZUSTAND of
        7 : begin
              FEHLER := TRUE ;
              FEHLERZEICHEN := EINGABEZEICHEN
            end (* tag/ca *) ;
        15 : begin
               FEHLER := TRUE ;
               FEHLERZEICHEN := EINGABEZEICHEN
             end (* tag/ca *) ;
        16 : begin
               FEHLER := TRUE ;
               FEHLERZEICHEN := EINGABEZEICHEN
             end (* tag/ca *) ;
        17 : begin
               FEHLER := TRUE ;
               FEHLERZEICHEN := EINGABEZEICHEN
             end (* tag/ca *) ;
        18 : begin
               FEHLER := TRUE ;
               FEHLERZEICHEN := EINGABEZEICHEN
             end (* tag/ca *) ;
        21 : begin
               FEHLER := TRUE ;
               FEHLERZEICHEN := EINGABEZEICHEN
             end (* tag/ca *) ;
        22 : begin
               FEHLER := TRUE ;
               FEHLERZEICHEN := EINGABEZEICHEN
             end (* tag/ca *) ;
        23 : begin
               FEHLER := TRUE ;
               FEHLERZEICHEN := EINGABEZEICHEN
             end (* tag/ca *) ;
        24 : begin
               FEHLER := TRUE ;
               FEHLERZEICHEN := EINGABEZEICHEN
             end (* tag/ca *) ;
        25 : begin
               FEHLER := TRUE ;
               FEHLERZEICHEN := EINGABEZEICHEN
             end (* tag/ca *) ;
        26 : begin
               FEHLER := TRUE ;
               FEHLERZEICHEN := EINGABEZEICHEN
             end (* tag/ca *) ;
        1 : begin
              if EINGABEZEICHEN in [ ' ' , '''' .. '/' , ':' .. ';' ,
              '=' , 'A' .. 'Z' , 'a' .. 'z' ] then
                case EINGABEZEICHEN of
                  ' ' : ZUSTAND := 2 ;
                  '''' : ZUSTAND := 8 ;
                  '(' : ZUSTAND := 17 ;
                  ')' : ZUSTAND := 18 ;
                  '*' : ZUSTAND := 24 ;
                  '+' : ZUSTAND := 22 ;
                  ',' : ZUSTAND := 16 ;
                  '-' : ZUSTAND := 23 ;
                  '.' : ZUSTAND := 20 ;
                  '/' : ZUSTAND := 27 ;
                  ':' : ZUSTAND := 14 ;
                  ';' : ZUSTAND := 25 ;
                  '=' : ZUSTAND := 26 ;
                  'A' , 'B' , 'C' , 'D' , 'E' , 'F' , 'G' , 'H' , 'I' ,
                  'J' , 'K' , 'L' , 'M' , 'N' , 'O' , 'P' , 'Q' , 'R' ,
                  'S' , 'T' , 'U' , 'V' , 'W' , 'X' , 'Y' , 'Z' , 'a' ,
                  'b' , 'c' , 'd' , 'e' , 'f' , 'g' , 'h' , 'i' , 'j' ,
                  'k' , 'l' , 'm' , 'n' , 'o' , 'p' , 'q' , 'r' , 's' ,
                  't' , 'u' , 'v' , 'w' , 'x' , 'y' , 'z' :
                    ZUSTAND := 12
                end (* case *)
              else
                begin
                  FEHLER := TRUE ;
                  FEHLERZEICHEN := EINGABEZEICHEN
                end (* else *)
            end (* tag/ca *) ;
        2 : begin
              if EINGABEZEICHEN in [ ' ' ] then
                case EINGABEZEICHEN of
                  ' ' : ZUSTAND := 2
                end (* case *)
              else
                begin
                  FEHLER := TRUE ;
                  FEHLERZEICHEN := EINGABEZEICHEN
                end (* else *)
            end (* tag/ca *) ;
        4 : begin
              if EINGABEZEICHEN in [ '#' , '*' ] then
                case EINGABEZEICHEN of
                  '#' : ZUSTAND := 5 ;
                  '*' : ZUSTAND := 28
                end (* case *)
              else
                begin
                  FEHLER := TRUE ;
                  FEHLERZEICHEN := EINGABEZEICHEN
                end (* else *)
            end (* tag/ca *) ;
        5 : begin
              if EINGABEZEICHEN in [ '#' , '*' ] then
                case EINGABEZEICHEN of
                  '#' : ZUSTAND := 5 ;
                  '*' : ZUSTAND := 28
                end (* case *)
              else
                begin
                  FEHLER := TRUE ;
                  FEHLERZEICHEN := EINGABEZEICHEN
                end (* else *)
            end (* tag/ca *) ;
        8 : begin
              if EINGABEZEICHEN in [ '#' , '''' ] then
                case EINGABEZEICHEN of
                  '#' : ZUSTAND := 9 ;
                  '''' : ZUSTAND := 29
                end (* case *)
              else
                begin
                  FEHLER := TRUE ;
                  FEHLERZEICHEN := EINGABEZEICHEN
                end (* else *)
            end (* tag/ca *) ;
        9 : begin
              if EINGABEZEICHEN in [ '#' , '''' ] then
                case EINGABEZEICHEN of
                  '#' : ZUSTAND := 9 ;
                  '''' : ZUSTAND := 29
                end (* case *)
              else
                begin
                  FEHLER := TRUE ;
                  FEHLERZEICHEN := EINGABEZEICHEN
                end (* else *)
            end (* tag/ca *) ;
        12 : begin
               if EINGABEZEICHEN in [ '0' .. '9' , 'A' .. 'Z' , '_' ,
               'a' .. 'z' ] then
                 case EINGABEZEICHEN of
                   '0' , '1' , '2' , '3' , '4' , '5' , '6' , '7' , '8'
                   , '9' , 'A' , 'B' , 'C' , 'D' , 'E' , 'F' , 'G' ,
                   'H' , 'I' , 'J' , 'K' , 'L' , 'M' , 'N' , 'O' , 'P'
                   , 'Q' , 'R' , 'S' , 'T' , 'U' , 'V' , 'W' , 'X' ,
                   'Y' , 'Z' , '_' , 'a' , 'b' , 'c' , 'd' , 'e' , 'f'
                   , 'g' , 'h' , 'i' , 'j' , 'k' , 'l' , 'm' , 'n' ,
                   'o' , 'p' , 'q' , 'r' , 's' , 't' , 'u' , 'v' , 'w'
                   , 'x' , 'y' , 'z' :
                     ZUSTAND := 13
                 end (* case *)
               else
                 begin
                   FEHLER := TRUE ;
                   FEHLERZEICHEN := EINGABEZEICHEN
                 end (* else *)
             end (* tag/ca *) ;
        13 : begin
               if EINGABEZEICHEN in [ '0' .. '9' , 'A' .. 'Z' , '_' ,
               'a' .. 'z' ] then
                 case EINGABEZEICHEN of
                   '0' , '1' , '2' , '3' , '4' , '5' , '6' , '7' , '8'
                   , '9' , 'A' , 'B' , 'C' , 'D' , 'E' , 'F' , 'G' ,
                   'H' , 'I' , 'J' , 'K' , 'L' , 'M' , 'N' , 'O' , 'P'
                   , 'Q' , 'R' , 'S' , 'T' , 'U' , 'V' , 'W' , 'X' ,
                   'Y' , 'Z' , '_' , 'a' , 'b' , 'c' , 'd' , 'e' , 'f'
                   , 'g' , 'h' , 'i' , 'j' , 'k' , 'l' , 'm' , 'n' ,
                   'o' , 'p' , 'q' , 'r' , 's' , 't' , 'u' , 'v' , 'w'
                   , 'x' , 'y' , 'z' :
                     ZUSTAND := 13
                 end (* case *)
               else
                 begin
                   FEHLER := TRUE ;
                   FEHLERZEICHEN := EINGABEZEICHEN
                 end (* else *)
             end (* tag/ca *) ;
        14 : if EINGABEZEICHEN = '=' then
               ZUSTAND := 15
             else
               begin
                 FEHLER := TRUE ;
                 FEHLERZEICHEN := EINGABEZEICHEN
               end (* else *) ;
        20 : if EINGABEZEICHEN = '.' then
               ZUSTAND := 21
             else
               begin
                 FEHLER := TRUE ;
                 FEHLERZEICHEN := EINGABEZEICHEN
               end (* else *) ;
        27 : if EINGABEZEICHEN = '*' then
               ZUSTAND := 4
             else
               begin
                 FEHLER := TRUE ;
                 FEHLERZEICHEN := EINGABEZEICHEN
               end (* else *) ;
        28 : begin
               if EINGABEZEICHEN in [ '#' , '*' , '/' ] then
                 case EINGABEZEICHEN of
                   '#' : ZUSTAND := 5 ;
                   '*' : ZUSTAND := 28 ;
                   '/' : ZUSTAND := 7
                 end (* case *)
               else
                 begin
                   FEHLER := TRUE ;
                   FEHLERZEICHEN := EINGABEZEICHEN
                 end (* else *)
             end (* tag/ca *) ;
        29 : if EINGABEZEICHEN = '''' then
               ZUSTAND := 9
             else
               begin
                 FEHLER := TRUE ;
                 FEHLERZEICHEN := EINGABEZEICHEN
               end (* else *)
      end (* case *) ;
      if EOLN ( INPUT ) then
        begin
          READLN ;
          WRITELN ;
        end (* then *) ;
      READ ( EINGABEZEICHEN ) ;
    end (* while *) ;
  WRITELN ;
  if FEHLER then
    if FEHLERZEICHEN in [ ' ' , '#' , '''' .. ';' , '=' ,
    'A' .. 'Z' , '_' , 'a' .. 'z' ] then
      WRITELN ( '**** FEHLER: AUTOMAT HAELT NICHT IN EINEM ENDZUSTAND'
                )
    else
      WRITELN ( '**** FEHLER: FALSCHES EINGABEZEICHEN' )
  else
    if ZUSTAND = 2 then
      WRITELN ( 'OK' )
    else
      if ZUSTAND = 7 then
        WRITELN ( 'OK' )
      else
        if ZUSTAND = 12 then
          WRITELN ( 'OK' )
        else
          if ZUSTAND = 13 then
            WRITELN ( 'OK' )
          else
            if ZUSTAND = 15 then
              WRITELN ( 'OK' )
            else
              if ZUSTAND = 16 then
                WRITELN ( 'OK' )
              else
                if ZUSTAND = 17 then
                  WRITELN ( 'OK' )
                else
                  if ZUSTAND = 18 then
                    WRITELN ( 'OK' )
                  else
                    if ZUSTAND = 21 then
                      WRITELN ( 'OK' )
                    else
                      if ZUSTAND = 22 then
                        WRITELN ( 'OK' )
                      else
                        if ZUSTAND = 23 then
                          WRITELN ( 'OK' )
                        else
                          if ZUSTAND = 24 then
                            WRITELN ( 'OK' )
                          else
                            if ZUSTAND = 25 then
                              WRITELN ( 'OK' )
                            else
                              if ZUSTAND = 26 then
                                WRITELN ( 'OK' )
                              else
                                if ZUSTAND = 27 then
                                  WRITELN ( 'OK' )
                                else
                                  if ZUSTAND = 29 then
                                    WRITELN ( 'OK' )
                                  else
                                    WRITELN (
                '**** FEHLER: AUTOMAT HAELT NICHT IN EINEM ENDZUSTAND'
                                              )
end (* HAUPTPROGRAMM *) .
