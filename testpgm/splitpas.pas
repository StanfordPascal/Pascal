program SPLITPAS ( INPUT , OUTPUT , OUTTEXT , OUTBIN ) ;

(***********************************************************)
(*                                                         *)
(*   This program splits the large download file           *)
(*   - see download.pas - into the original seperate       *)
(*   files and writes them (on Windows etc.) into          *)
(*   directories; the names of the directories are         *)
(*   built from the names of the original MVS datasets.    *)
(*                                                         *)
(*   The hex flags are recognized; the hex encoded         *)
(*   files are written in the original binary              *)
(*   representation, so that they could be transferred     *)
(*   to a target mainframe using binary FTP or             *)
(*   equivalent transfer protocols.                        *)
(*                                                         *)
(***********************************************************)
(*                                                         *)
(*   Author: Bernd Oppolzer - May 2017                     *)
(*                                                         *)
(***********************************************************)
(*                                                         *)
(*   outtext = output file for text                        *)
(*   outbin  = output file for binary (recfm f lrecl 80)   *)
(*                                                         *)
(*   the program uses the new anyfile type;                *)
(*   this was needed to make assign on binary files        *)
(*   possible                                              *)
(*                                                         *)
(*   assign assigns Windows file names (and paths)         *)
(*   to Pascal files (before rewrite in this case)         *)
(*                                                         *)
(***********************************************************)



const SIZEDSN = 44 ;
      SIZEMEM = 8 ;
      SIZEEXT = 3 ;
      MAXLINEOFFS = 72 ;
      MAXOUT = 100 ;


type CHARPTR = -> CHAR ;
     CHAR4 = array [ 1 .. 4 ] of CHAR ;
     CHAR80 = array [ 1 .. 80 ] of CHAR ;
     ZEILE = array [ 1 .. 128 ] of CHAR ;
     RECBIN = record
                Z : CHAR80 ;
              end ;


var OUTTEXT : TEXT ;
    OUTBIN : FILE of RECBIN ;
    OUTREC : RECBIN ;
    ZINP : ZEILE ;
    ZOUT : array [ 1 .. 1000 ] of CHAR ;
    LOUT : INTEGER ;
    OUTPOS : INTEGER ;
    CPOUT : CHARPTR ;
    TAG : CHAR4 ;
    DSN : array [ 1 .. SIZEDSN ] of CHAR ;
    MEM : array [ 1 .. SIZEMEM ] of CHAR ;
    EXT : array [ 1 .. SIZEEXT ] of CHAR ;
    HEXFLAG : CHAR ;



procedure ASSIGN ( var X : ANYFILE ; FNAME : CHARPTR ; LEN : INTEGER )
                 ;

   EXTERNAL ;



procedure ASSIGN_FILE ( PDSN : CHARPTR ; PMEM : CHARPTR ; PEXT :
                      CHARPTR ; HEXFLAG : CHAR ) ;

(***********************************************************)
(*                                                         *)
(*   this procedure assigns a file name and path           *)
(*   to the pascal file OUTBIN or OUTTEXT                  *)
(*   (depending on the HEXFLAG). The directory is          *)
(*   built from the DSN, the file name is built from       *)
(*   the member name and the extension                     *)
(*                                                         *)
(***********************************************************)


   var DSN : array [ 1 .. SIZEDSN ] of CHAR ;
       MEM : array [ 1 .. SIZEMEM ] of CHAR ;
       EXT : array [ 1 .. SIZEEXT ] of CHAR ;
       PFADNAME : array [ 1 .. SIZEDSN ] of CHAR ;
       PFADLEN : INTEGER ;
       FILENAME : array [ 1 .. 100 ] of CHAR ;
       FILELEN : INTEGER ;
       I : INTEGER ;
       MD_CMD : array [ 1 .. 100 ] of CHAR ;
       RC : INTEGER ;
       CMDPART : CHAR80 ;

   begin (* ASSIGN_FILE *)
     MEMCPY ( ADDR ( DSN ) , PDSN , SIZEDSN ) ;
     MEMCPY ( ADDR ( MEM ) , PMEM , SIZEMEM ) ;
     MEMCPY ( ADDR ( EXT ) , PEXT , SIZEEXT ) ;
     WRITELN ( '-----------------------------' ) ;
     WRITELN ( 'dsn = ' , DSN ) ;
     WRITELN ( 'mem = ' , MEM ) ;
     WRITELN ( 'ext = ' , EXT ) ;
     WRITELN ( 'hex = ' , HEXFLAG ) ;
     WRITELN ( '-----------------------------' ) ;
     PFADLEN := 0 ;
     PFADNAME := DSN ;
     for I := 1 to SIZEDSN do
       if PFADNAME [ I ] <> ' ' then
         begin
           PFADLEN := I ;
           if PFADNAME [ I ] = '.' then
             PFADNAME [ I ] := '_'
         end (* then *) ;
     WRITELN ( 'pfadname = ' , PFADNAME ) ;
     WRITELN ( 'pfadlen = ' , PFADLEN ) ;

     /********************************************************/
     /*  create directory                                    */
     /********************************************************/

     MD_CMD := 'mkdir ' ;
     MEMCPY ( ADDR ( MD_CMD [ 7 ] ) , ADDR ( PFADNAME ) , SIZEDSN ) ;
     CMDPART := '2>NUL #' ;
     MEMCPY ( ADDR ( MD_CMD [ 53 ] ) , ADDR ( CMDPART ) , 8 ) ;
     WINX ( ADDR ( MD_CMD ) , RC ) ;

     /********************************************************/
     /*  ASSign file                                         */
     /********************************************************/

     FILENAME := ' ' ;
     MEMCPY ( ADDR ( FILENAME ) , ADDR ( PFADNAME ) , SIZEDSN ) ;
     FILENAME [ PFADLEN + 1 ] := '/' ;
     MEMCPY ( ADDR ( FILENAME [ PFADLEN + 2 ] ) , ADDR ( MEM ) ,
              SIZEMEM ) ;
     for I := PFADLEN + 2 to PFADLEN + 10 do
       if FILENAME [ I ] = ' ' then
         begin
           FILELEN := I - 1 ;
           break
         end (* then *) ;
     FILENAME [ FILELEN + 1 ] := '.' ;
     MEMCPY ( ADDR ( FILENAME [ FILELEN + 2 ] ) , ADDR ( EXT ) ,
              SIZEEXT ) ;
     FILELEN := FILELEN + 1 + SIZEEXT ;
     WRITELN ( 'filename = ' , FILENAME ) ;
     WRITELN ( 'filelen = ' , FILELEN ) ;
     WRITELN ( '-----------------------------' ) ;
     if HEXFLAG = 'H' then
       ASSIGN ( OUTBIN , ADDR ( FILENAME ) , FILELEN )
     else
       ASSIGN ( OUTTEXT , ADDR ( FILENAME ) , FILELEN ) ;
   end (* ASSIGN_FILE *) ;



procedure WRITEBUF ( HEXFLAG : CHAR ; CPOUT : CHARPTR ; LOUT : INTEGER
                   ) ;

   var I : INTEGER ;
       H : INTEGER ;
       HEX : INTEGER ;
       CH : CHAR ;
       CPLAST : CHARPTR ;

       (***********************************************************)
       (*                                                         *)
       (*   the buffer (addressed by cpout, length in lout)       *)
       (*   is written to the output file.                        *)
       (*                                                         *)
       (*   if hex, the decoding is done.                         *)
       (*                                                         *)
       (*   if not hex, trailing blanks are eliminated.           *)
       (*                                                         *)
       (***********************************************************)


   begin (* WRITEBUF *)
     if HEXFLAG = 'H' then
       begin
         OUTREC . Z := '' ;
         for I := 1 to 80 do
           begin
             CH := CPOUT -> ;
             if ( CH >= '0' ) and ( CH <= '9' ) then
               H := ORD ( CH ) - ORD ( '0' )
             else
               if ( CH >= 'A' ) and ( CH <= 'F' ) then
                 H := ORD ( CH ) - ORD ( 'A' ) + 10 ;
             HEX := H * 16 ;
             CPOUT := PTRADD ( CPOUT , 1 ) ;
             CH := CPOUT -> ;
             if ( CH >= '0' ) and ( CH <= '9' ) then
               H := ORD ( CH ) - ORD ( '0' )
             else
               if ( CH >= 'A' ) and ( CH <= 'F' ) then
                 H := ORD ( CH ) - ORD ( 'A' ) + 10 ;
             HEX := HEX + H ;
             CPOUT := PTRADD ( CPOUT , 1 ) ;
             OUTREC . Z [ I ] := CHR ( HEX ) ;
           end (* for *) ;

     /***************************************/
     /* PUT ( OUTBIN ) ;                    */
     /* OUTBIN -> := OUTREC ;               */
     /***************************************/

         WRITE ( OUTBIN , OUTREC ) ;
       end (* then *)
     else
       begin
         CPLAST := PTRADD ( CPOUT , LOUT - 1 ) ;
         while ( LOUT > 0 ) and ( CPLAST -> = ' ' ) do
           begin
             LOUT := LOUT - 1 ;
             CPLAST := PTRADD ( CPLAST , - 1 ) ;
           end (* while *) ;
         for I := 1 to LOUT do
           begin
             WRITE ( OUTTEXT , CPOUT -> ) ;
             CPOUT := PTRADD ( CPOUT , 1 ) ;
           end (* for *) ;
         WRITELN ( OUTTEXT ) ;
       end (* else *)
   end (* WRITEBUF *) ;



begin (* HAUPTPROGRAMM *)
  READLN ( ZINP ) ;
  while not EOF ( INPUT ) do
    begin
      MEMCPY ( ADDR ( TAG ) , ADDR ( ZINP ) , 4 ) ;

  (***********************************************************)
  (*   the record tagged with FILE contains the              *)
  (*   meta information (DSN, MEM, EXT, Hex Flag)            *)
  (***********************************************************)

      if TAG = 'FILE' then
        begin
          MEMCPY ( ADDR ( DSN ) , ADDR ( ZINP [ 6 ] ) , SIZEDSN ) ;
          MEMCPY ( ADDR ( MEM ) , ADDR ( ZINP [ 56 ] ) , SIZEMEM ) ;
          MEMCPY ( ADDR ( EXT ) , ADDR ( ZINP [ 69 ] ) , SIZEEXT ) ;
          HEXFLAG := ZINP [ 77 ] ;
          ASSIGN_FILE ( ADDR ( DSN ) , ADDR ( MEM ) , ADDR ( EXT ) ,
                        HEXFLAG ) ;
          if HEXFLAG = 'H' then
            REWRITE ( OUTBIN )
          else
            REWRITE ( OUTTEXT ) ;
          LOUT := - 1 ;
          repeat
            if EOF ( INPUT ) then
              break ;
            READLN ( ZINP ) ;
            MEMCPY ( ADDR ( TAG ) , ADDR ( ZINP ) , 4 ) ;

  (***********************************************************)
  (*   the other records (DATA) are collected into           *)
  (*   the large buffer ZOUT. When the buffer is complete,   *)
  (*   WRITEBUF is called to flush the buffer.               *)
  (***********************************************************)

            if TAG = 'DATA' then
              begin
                if ZINP [ 5 ] = '1' then
                  begin
                    if LOUT > 0 then
                      WRITEBUF ( HEXFLAG , ADDR ( ZOUT ) , LOUT ) ;
                    LOUT := IVALSTR ( ADDR ( ZINP [ 6 ] ) , 5 ) ;
                    OUTPOS := IVALSTR ( ADDR ( ZINP [ 11 ] ) , 5 ) ;
                    if HEXFLAG = 'H' then
                      OUTPOS := OUTPOS * 2 ;
                    CPOUT := PTRADD ( ADDR ( ZOUT ) , OUTPOS ) ;
                    MEMCPY ( CPOUT , ADDR ( ZINP [ 17 ] ) , 100 ) ;
                  end (* then *)
                else
                  begin
                    OUTPOS := IVALSTR ( ADDR ( ZINP [ 11 ] ) , 5 ) ;
                    if HEXFLAG = 'H' then
                      OUTPOS := OUTPOS * 2 ;
                    CPOUT := PTRADD ( ADDR ( ZOUT ) , OUTPOS ) ;
                    MEMCPY ( CPOUT , ADDR ( ZINP [ 17 ] ) , 100 ) ;
                  end (* else *)
              end (* then *)
          until TAG <> 'DATA' ;
          if LOUT > 0 then
            WRITEBUF ( HEXFLAG , ADDR ( ZOUT ) , LOUT ) ;
          if HEXFLAG = 'H' then
            CLOSE ( OUTBIN )
          else
            CLOSE ( OUTTEXT ) ;
        end (* then *)
      else
        begin
          WRITELN ( '+++ falsche Satzart: ' , TAG ) ;
          break ;
        end (* else *)
    end (* while *)
end (* HAUPTPROGRAMM *) .
