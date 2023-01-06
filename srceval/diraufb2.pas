program DIRAUFB2 ( DIROUT , DIROUT2 ) ;

//*********************************************************
// Aus DIROUT-File (siehe DIRAUFB) werden die ENDDIR Zeilen
// herausgeholt und die Groessenangaben pro Untervezeichnis
// aufsummiert und den darueberliegenden Verzeichnissen    
// zugeordnet                                              
//*********************************************************
// Autor: Bernd Oppolzer - November 2022                   
// New Stanford Pascal                                     
//*********************************************************



const MAXSUB = 20 ;   // maximum number of subdirs in path


var DIROUT : TEXT ;
    DIROUT2 : TEXT ;
    DIRZEILE : STRING ( 250 ) ;
    AKTDIR : STRING ( 250 ) ;
    GROESSE : STRING ( 20 ) ;
    GROESSEDIR : DECIMAL ( 15 ) ;
    GROESSETOTAL : array [ 1 .. MAXSUB ] of DECIMAL ( 15 ) ;
    X : INTEGER ;
    NR : INTEGER ;



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



function GLEICHER_PFAD ( const P1 , P2 : STRING ) : BOOLEAN ;

   var X1 : STRING ( 250 ) ;
       X2 : STRING ( 250 ) ;
       IX1 , IX2 : INTEGER ;

   begin (* GLEICHER_PFAD *)
     if LENGTH ( P1 ) < LENGTH ( P2 ) then
       begin
         X1 := P1 ;
         X2 := P2
       end (* then *)
     else
       begin
         X1 := P2 ;
         X2 := P1
       end (* else *) ;

     //*************************************************
     // X1 now contains the shorter of the two strings  
     // if x1 equal to left substr of x2, then x1 is    
     // super directory of x2                           
     //*************************************************

     if SUBSTR ( X2 , 1 , LENGTH ( X1 ) ) = X1 then
       begin
         GLEICHER_PFAD := TRUE ;
         return
       end (* then *) ;

     //*********************************************
     // if not, eliminate last subdir of x1 and x2  
     // and check, if the rest is equal             
     //*********************************************

     IX1 := LENGTH ( X1 ) ;
     while X1 [ IX1 ] <> '\' do
       IX1 := IX1 - 1 ;
     IX2 := LENGTH ( X2 ) ;
     while X2 [ IX2 ] <> '\' do
       IX2 := IX2 - 1 ;
     if IX1 <> IX2 then
       begin
         GLEICHER_PFAD := FALSE ;
         return
       end (* then *) ;
     GLEICHER_PFAD := SUBSTR ( X1 , 1 , IX1 ) = SUBSTR ( X2 , 1 , IX2 )
                      ;
   end (* GLEICHER_PFAD *) ;



function NROFSEPS ( const P : STRING ) : INTEGER ;

   var IX : INTEGER ;
       C : INTEGER := 0 ;

   begin (* NROFSEPS *)
     for IX := 1 to LENGTH ( P ) do
       if P [ IX ] = '\' then
         C := C + 1 ;
     NROFSEPS := C
   end (* NROFSEPS *) ;



begin (* HAUPTPROGRAMM *)
  for X := 1 to MAXSUB do
    GROESSETOTAL [ X ] := 0 ;
  RESET ( DIROUT ) ;
  REWRITE ( DIROUT2 ) ;
  while not EOF ( DIROUT ) do
    begin

  //************************************
  // zeile einlesen von directory-file  
  //************************************

      READLN ( DIROUT , DIRZEILE ) ;
      if LEFT ( DIRZEILE , 8 ) <> 'EndDir: ' then
        continue ;

  //***********************************
  // enddir-zeile mit groessenangaben  
  //***********************************

      X := INDEX ( DIRZEILE , ' Size = ' ) ;
      AKTDIR := SUBSTR ( DIRZEILE , 9 , X - 8 ) ;
      AKTDIR := TRIM ( AKTDIR ) ;

  //*****************************************************
  // wenn das aktuelle verzeichnis im gleichen baum ist  
  // wie das verzeichnis davor, dann aufsummieren        
  // ansonsten groessetotal auf Null setzen              
  //*****************************************************

      X := X + 7 ;
      GROESSE := SUBSTR ( DIRZEILE , X ) ;
      GROESSEDIR := DVAL ( GROESSE ) ;

  //*****************************************************
  // check number of dir separators in path name         
  // this number defines which sums have to be           
  // resetted when printing                              
  //*****************************************************

      NR := NROFSEPS ( AKTDIR ) ;
      for X := 1 to NR do
        GROESSETOTAL [ X ] := GROESSETOTAL [ X ] + GROESSEDIR ;
      for X := NR + 1 to MAXSUB do
        GROESSETOTAL [ X ] := 0 ;
      AKTDIR := LEFT ( AKTDIR , 50 ) ;
      WRITELN ( DIROUT2 , AKTDIR , GROESSE : 15 , GROESSETOTAL [ NR ] :
                15 ) ;
      GROESSETOTAL [ NR ] := 0 ;
    end (* while *) ;
end (* HAUPTPROGRAMM *) .
