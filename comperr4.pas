program COMPERR4 ( OUTPUT ) ;

//***********************************
// tut komischerweise                
// fehler von cobform reproduzieren  
//***********************************



var SNEU : STRING ( 70 ) ;



function S1 ( const S : STRING ) : STRING ;

   begin (* S1 *)
     S1 := TRIM ( S )
   end (* S1 *) ;



function S2 ( const S : STRING ) : STRING ;

   begin (* S2 *)
     S2 := TRIM ( S )
   end (* S2 *) ;



begin (* HAUPTPROGRAMM *)
  WRITELN ( 's1 = ' , S1 ( 'Bernd ' ) ) ;
  WRITELN ( 's2 = ' , S2 ( 'Oppolzer' ) ) ;
  SNEU := S1 ( 'Sissi ' ) || ' ' || S2 ( 'Karst-Oppolzer' ) ;
  WRITELN ( 'res = ' , SNEU ) ;
  SNEU := S1 ( 'Sissi ' ) || ' ' || TRIM ( 'Karst-Oppolzer' ) ;
  WRITELN ( 'res = ' , SNEU ) ;
end (* HAUPTPROGRAMM *) .
