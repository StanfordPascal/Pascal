program COMPERR4 ( OUTPUT ) ;

//***********************************
//$A+
//***********************************



var SNEU : STRING ( 70 ) ;



function BLANKS_LEVEL ( X : INTEGER ; ZONE : CHAR ) : STRING ;

//**********************************************************
// generate blanks depending on level
// x = computed nesting level
// zone = zone a or b
// if x > 1 and zone a, additional indentation needed
//**********************************************************


   var N : STRING ( 80 ) ;

   begin (* BLANKS_LEVEL *)
     N := '' ;
     while X > 2 do
       begin
         N := N || '    ' ;
         X := X - 1
       end (* while *) ;
     if X > 1 then
       if ZONE = 'A' then
         N := N || '    ' ;
     BLANKS_LEVEL := N
   end (* BLANKS_LEVEL *) ;



function S1 ( const S : STRING ) : STRING ;

   begin (* S1 *)
     S1 := S
   end (* S1 *) ;



function S2 ( const S : STRING ) : STRING ;

   begin (* S2 *)
     S2 := TRIM ( S )
   end (* S2 *) ;



begin (* HAUPTPROGRAMM *)

  //*******************************************************
  // testen left mit leerem string ... scheint zu klappen
  //*******************************************************

  SNEU := LEFT ( ' ' , 4 ) ;
  WRITELN ( 'res = <' , SNEU , '>' ) ;
  SNEU := LEFT ( '' , 4 ) ;
  WRITELN ( 'res = <' , SNEU , '>' ) ;

  //********************************************************************
  // bei den folgenden Ausgaben sollte immer irgendein Name rauskommen
  //********************************************************************

  WRITELN ( 's1 = ' , S1 ( 'Bernd ' ) ) ;
  WRITELN ( 's2 = ' , S2 ( 'Oppolzer' ) ) ;

  //********************************************************************
  // bei den folgenden Ausgaben sollte immer irgendein Name rauskommen
  //********************************************************************

  WRITELN ( 'Karst-Oppolzer sollte rauskommen' ) ;
  SNEU := BLANKS_LEVEL ( 3 , 'A' ) ;
  WRITELN ( 'res = <' , SNEU , '>' ) ;
  SNEU := S2 ( 'Karst-Oppolzer' ) ;
  WRITELN ( 'res = <' , SNEU , '>' ) ;
  SNEU := BLANKS_LEVEL ( 3 , 'A' ) || S2 ( 'Karst-Oppolzer' ) ;
  WRITELN ( 'res = <' , SNEU , '>' ) ;

  //********************************************************************
  // hier klappt's
  //********************************************************************

  WRITELN ( 'hier klappt''s' ) ;
  SNEU := BLANKS_LEVEL ( 3 , 'A' ) ;
  SNEU := SNEU || S2 ( 'Karst-Oppolzer' ) ;
  WRITELN ( 'res = <' , SNEU , '>' ) ;

  //********************************************************************
  // hier wieder nicht
  //********************************************************************

  WRITELN ( 'hier wieder nicht' ) ;
  SNEU := BLANKS_LEVEL ( 3 , 'A' ) || TRIM ( 'Karst-Oppolzer' ) ;
  WRITELN ( 'res = <' , SNEU , '>' ) ;

  //********************************************************************
  // hier klappt's wieder
  //********************************************************************

  WRITELN ( 'hier klappt''s wieder' ) ;
  SNEU := BLANKS_LEVEL ( 3 , 'A' ) ;
  SNEU := SNEU || TRIM ( 'Karst-Oppolzer' ) ;
  WRITELN ( 'res = <' , SNEU , '>' ) ;
end (* HAUPTPROGRAMM *) .
