program COMPERR4 ( OUTPUT ) ;

//***********************************
// tut komischerweise                
// fehler von cobform reproduzieren  
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
  SNEU := LEFT ( ' ' , 4 ) ;
  WRITELN ( 'res = <' , SNEU , '>' ) ;
  SNEU := LEFT ( '' , 4 ) ;
  WRITELN ( 'res = <' , SNEU , '>' ) ;
  WRITELN ( 's1 = ' , S1 ( 'Bernd ' ) ) ;
  WRITELN ( 's2 = ' , S2 ( 'Oppolzer' ) ) ;
  SNEU := BLANKS_LEVEL ( 3 , 'A' ) || S2 ( 'Karst-Oppolzer' ) ;
  WRITELN ( 'res = <' , SNEU , '>' ) ;
  SNEU := BLANKS_LEVEL ( 3 , 'A' ) ;
  SNEU := SNEU || S2 ( 'Karst-Oppolzer' ) ;
  WRITELN ( 'res = <' , SNEU , '>' ) ;
  SNEU := BLANKS_LEVEL ( 3 , 'A' ) || TRIM ( 'Karst-Oppolzer' ) ;
  WRITELN ( 'res = <' , SNEU , '>' ) ;
  SNEU := BLANKS_LEVEL ( 3 , 'A' ) ;
  SNEU := SNEU || TRIM ( 'Karst-Oppolzer' ) ;
  WRITELN ( 'res = <' , SNEU , '>' ) ;
end (* HAUPTPROGRAMM *) .
