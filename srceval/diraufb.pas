program DIRAUFB ( DIRFILE , DIROUT ) ;

//*********************************************************
// Programm zum Aufbereiten von DIR-Ausgaben (rekursiv)    
// Dadurch lassen sich unterschiedliche Verzeichnisbaeume  
// z.B. Original und Backup                                
// leichter vergleichen und Unterschiede z.B. im Verlauf   
// eines Arbeitstages leichter erkennen                    
//*********************************************************
// Autor: Bernd Oppolzer - August 2020                     
// New Stanford Pascal                                     
//*********************************************************



var DIRFILE : TEXT ;
    DIROUT : TEXT ;
    DIRZEILE : STRING ( 250 ) ;
    AKTDIR : STRING ( 250 ) ;
    GROESSEDIR : DECIMAL ( 15 ) ;
    GROESSETOTAL : DECIMAL ( 15 ) ;
    FNAME_WIDTH : INTEGER ;



function DVAL ( const S : STRING ) : DECIMAL ( 13 ) ;

   var D : DECIMAL ( 13 ) ;
       I : INTEGER ;

   begin (* DVAL *)
     D := 0.0 ;
     for I := 1 to LENGTH ( S ) do
       if S [ I ] in [ '0' .. '9' ] then
         D := 10.0 * D + ORD ( S [ I ] ) - ORD ( '0' ) ;
     DVAL := D
   end (* DVAL *) ;



function CHKPARM : INTEGER ;

/***********************************/
/* check run time parms via osparm */
/* parm = width of file name       */
/* default = 25                    */
/***********************************/


   var SX : INTEGER ;
       SZ : INTEGER ;
       CH : CHAR ;

   begin (* CHKPARM *)
     if OSPARM = NIL then
       begin
         CHKPARM := 25 ;
         return ;
       end (* then *) ;
     SZ := 0 ;
     with OSPARM -> do
       for SX := 1 to PLENGTH do
         begin
           CH := PSTRING [ SX ] ;
           if CH in [ '0' .. '9' ] then
             SZ := 10 * SZ + ORD ( CH ) - ORD ( '0' ) ;
         end (* for *) ;
     CHKPARM := SZ ;
   end (* CHKPARM *) ;



procedure VOLUME ( const X : STRING ) ;

   begin (* VOLUME *)
     WRITELN ( DIROUT , 'Volume: ' , X ) ;
   end (* VOLUME *) ;



procedure VOLUMESERIE ( const X : STRING ) ;

   begin (* VOLUMESERIE *)
     WRITELN ( DIROUT , 'Serial: ' , X ) ;
   end (* VOLUMESERIE *) ;



procedure VERZEICHNIS_ENDE ;

   begin (* VERZEICHNIS_ENDE *)
     if AKTDIR <> '' then
       WRITELN ( DIROUT , 'EndDir: ' , AKTDIR , ' Size = ' , GROESSEDIR
                 : 1 ) ;
     GROESSETOTAL := GROESSETOTAL + GROESSEDIR ;
     GROESSEDIR := 0 ;
   end (* VERZEICHNIS_ENDE *) ;



procedure VERZEICHNIS ( const X : STRING ) ;

   begin (* VERZEICHNIS *)
     VERZEICHNIS_ENDE ;
     AKTDIR := SUBSTR ( X , 18 ) ;
     WRITELN ( DIROUT , 'Direct: ' , AKTDIR ) ;
   end (* VERZEICHNIS *) ;



procedure NORMALFILE ( const X : STRING ) ;

   var DATUM : CHAR ( 10 ) ;
       DATUM_EUR : CHAR ( 10 ) ;
       UHRZEIT : CHAR ( 5 ) ;
       SGROESSE : CHAR ( 15 ) ;
       GROESSE : DECIMAL ( 13 ) ;
       FNAME : STRING ( 250 ) ;

   begin (* NORMALFILE *)
     DATUM := SUBSTR ( X , 1 , 10 ) ;
     DATUM_EUR := SUBSTR ( DATUM , 7 , 4 ) || '-' || SUBSTR ( DATUM , 4
                  , 2 ) || '-' || SUBSTR ( DATUM , 1 , 2 ) ;
     UHRZEIT := SUBSTR ( X , 13 , 5 ) ;
     SGROESSE := SUBSTR ( X , 21 , 15 ) ;
     FNAME := SUBSTR ( X , 37 ) ;
     if FNAME = '.' then
       return ;
     if FNAME = '..' then
       return ;
     if SGROESSE = ' <DIR>' then
       return ;
     GROESSE := DVAL ( STR ( SGROESSE ) ) ;
     WRITELN ( DIROUT , 'Normal: ' , DATUM_EUR , ' ' , UHRZEIT , ' ' ,
               GROESSE : 13 , ' ' , FNAME : - FNAME_WIDTH , ' ' ,
               AKTDIR ) ;
     GROESSEDIR := GROESSEDIR + GROESSE ;
   end (* NORMALFILE *) ;



begin (* HAUPTPROGRAMM *)
  FNAME_WIDTH := CHKPARM ;
  RESET ( DIRFILE ) ;
  REWRITE ( DIROUT ) ;
  AKTDIR := '' ;
  while not EOF ( DIRFILE ) do
    begin
      READLN ( DIRFILE , DIRZEILE ) ;
      if LENGTH ( DIRZEILE ) > 17 then
        begin
          if LEFT ( DIRZEILE , 8 ) = ' Volume ' then
            VOLUME ( DIRZEILE )
          else
            if LEFT ( DIRZEILE , 12 ) = ' Volumeserie' then
              VOLUMESERIE ( DIRZEILE )
            else
              if LEFT ( DIRZEILE , 13 ) = ' Verzeichnis ' then
                VERZEICHNIS ( DIRZEILE )
              else
                if ( DIRZEILE [ 3 ] = '.' ) and ( DIRZEILE [ 6 ] = '.'
                ) then
                  NORMALFILE ( DIRZEILE )
        end (* then *)
    end (* while *) ;
  VERZEICHNIS_ENDE ;
  WRITELN ( DIROUT , 'EndTot: ' , 'Size = ' , GROESSETOTAL : 1 ) ;
end (* HAUPTPROGRAMM *) .
