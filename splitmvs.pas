program SPLITMVS ( INPUT , OUTPUT , OUTF001 , OUTF002 , OUTF003 ,
                   OUTF004 , OUTF005 , OUTF006 , OUTF007 , OUTF008 ,
                   OUTF009 , OUTF00A , OUTF00B , OUTF00C , OUTF00D ,
                   OUTF00E , OUTF00F ) ;

(***********************************************************)
(*                                                         *)
(*   This program creates on MVS all the files             *)
(*   of the Pascal system from the large download file     *)
(*   named PASCALN.TXT.                                    *)
(*                                                         *)
(*   It is similar to SPLITPAS.PAS, which creates          *)
(*   directories on Windows from the same file.            *)
(*                                                         *)
(***********************************************************)
(*                                                         *)
(*   Author: Bernd Oppolzer - December 2017                *)
(*                                                         *)
(***********************************************************)



const SIZEDSN = 44 ;
      SIZEMEM = 8 ;
      SIZEEXT = 3 ;
      MAXLINEOFFS = 72 ;
      MAXOUT = 100 ;


type CHARPTR = -> CHAR ;
     ZEILE = CHAR ( 128 ) ;
     DSNAME = CHAR ( SIZEDSN ) ;


var OUTF001 : TEXT ;
    OUTF002 : TEXT ;
    OUTF003 : TEXT ;
    OUTF004 : TEXT ;
    OUTF005 : TEXT ;
    OUTF006 : TEXT ;
    OUTF007 : TEXT ;
    OUTF008 : TEXT ;
    OUTF009 : TEXT ;
    OUTF00A : TEXT ;
    OUTF00B : TEXT ;
    OUTF00C : TEXT ;
    OUTF00D : TEXT ;
    OUTF00E : TEXT ;
    OUTF00F : TEXT ;
    FN : CHAR ;
    ZINP : ZEILE ;
    ZOUT : CHAR ( 1000 ) ;
    LOUT : INTEGER ;
    OUTPOS : INTEGER ;
    CPIN : CHARPTR ;
    CPOUT : CHARPTR ;
    TAG : CHAR ( 6 ) ;
    DSN : CHAR ( SIZEDSN ) ;
    MEM : CHAR ( SIZEMEM ) ;
    EXT : CHAR ( SIZEEXT ) ;
    HEXFLAG : CHAR ;



procedure ASSIGN_FILE ( PDSN : CHARPTR ; PMEM : CHARPTR ; PEXT :
                      CHARPTR ; HEXFLAG : CHAR ) ;

(***********************************************************)
(*                                                         *)
(*   this procedure assigns a file name and path           *)
(*   to the pascal file OUTBIN or OUTF001                  *)
(*   (depending on the HEXFLAG). The directory is          *)
(*   built from the DSN, the file name is built from       *)
(*   the member name and the extension                     *)
(*                                                         *)
(***********************************************************)


   var DSN : CHAR ( SIZEDSN ) ;
       MEM : CHAR ( SIZEMEM ) ;
       EXT : CHAR ( SIZEEXT ) ;
       PFADNAME : CHAR ( SIZEDSN ) ;
       PFADLEN : INTEGER ;
       FILENAME : CHAR ( 100 ) ;
       FILELEN : INTEGER ;
       I : INTEGER ;
       MD_CMD : CHAR ( 100 ) ;
       RC : INTEGER ;
       CMDPART : CHAR ( 80 ) ;

   const DSN_TAB : array [ 1 .. 15 ] of CHAR ( SIZEDSN ) =
         ( 'PASCALN.COMPILER.PAS                    ' ,
           'PASCALN.COMPILER.CNTL                   ' ,
           'PASCALN.COMPILER.MESSAGES               ' ,
           'PASCALN.COMPILER.PROCLIB                ' ,
           'PASCALN.RUNTIME.ASM                     ' ,
           'PASCALN.TESTPGM.ASM                     ' ,
           'PASCALN.TESTPGM.PAS                     ' ,
           'PASCALN.TESTPGM.CNTL                    ' ,
           'PASCALN.COMPILER.TEXT                   ' ,
           'PASCALN.RUNTIME.TEXT                    ' ,
           'PASCALN.RUNTIME.MATHTEXT                ' ,
           'PASCALN.OLDCOMP.CNTL                    ' ,
           'PASCALN.OLDCOMP.SAMPLE                  ' ,
           'PASCALN.OLDCOMP.SOURCE                  ' ,
           '                                        ' ) ;
         FN_TAB : array [ 1 .. 15 ] of CHAR = '123456789ABCDEF' ;

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

     /********************************************************/
     /*  set fn (global char) depeding on filename           */
     /*  read from input file pascaln.txt                    */
     /********************************************************/

     FN := ' ' ;
     for I := 1 to 15 do
       if DSN = DSN_TAB [ I ] then
         begin
           FN := FN_TAB [ I ] ;
           break
         end (* then *) ;

     /********************************************************/
     /*  ASSign member and open file for writing             */
     /********************************************************/

     case FN of
       '1' : begin
               ASSIGNMEM ( OUTF001 , ADDR ( MEM ) , 8 ) ;
               REWRITE ( OUTF001 ) ;
             end (* tag/ca *) ;
       '2' : begin
               ASSIGNMEM ( OUTF002 , ADDR ( MEM ) , 8 ) ;
               REWRITE ( OUTF002 ) ;
             end (* tag/ca *) ;
       '3' : begin
               ASSIGNMEM ( OUTF003 , ADDR ( MEM ) , 8 ) ;
               REWRITE ( OUTF003 ) ;
             end (* tag/ca *) ;
       '4' : begin
               ASSIGNMEM ( OUTF004 , ADDR ( MEM ) , 8 ) ;
               REWRITE ( OUTF004 ) ;
             end (* tag/ca *) ;
       '5' : begin
               ASSIGNMEM ( OUTF005 , ADDR ( MEM ) , 8 ) ;
               REWRITE ( OUTF005 ) ;
             end (* tag/ca *) ;
       '6' : begin
               ASSIGNMEM ( OUTF006 , ADDR ( MEM ) , 8 ) ;
               REWRITE ( OUTF006 ) ;
             end (* tag/ca *) ;
       '7' : begin
               ASSIGNMEM ( OUTF007 , ADDR ( MEM ) , 8 ) ;
               REWRITE ( OUTF007 ) ;
             end (* tag/ca *) ;
       '8' : begin
               ASSIGNMEM ( OUTF008 , ADDR ( MEM ) , 8 ) ;
               REWRITE ( OUTF008 ) ;
             end (* tag/ca *) ;
       '9' : begin
               ASSIGNMEM ( OUTF009 , ADDR ( MEM ) , 8 ) ;
               REWRITE ( OUTF009 ) ;
             end (* tag/ca *) ;
       'A' : begin
               ASSIGNMEM ( OUTF00A , ADDR ( MEM ) , 8 ) ;
               REWRITE ( OUTF00A ) ;
             end (* tag/ca *) ;
       'B' : begin
               ASSIGNMEM ( OUTF00B , ADDR ( MEM ) , 8 ) ;
               REWRITE ( OUTF00B ) ;
             end (* tag/ca *) ;
       'C' : begin
               ASSIGNMEM ( OUTF00C , ADDR ( MEM ) , 8 ) ;
               REWRITE ( OUTF00C ) ;
             end (* tag/ca *) ;
       'D' : begin
               ASSIGNMEM ( OUTF00D , ADDR ( MEM ) , 8 ) ;
               REWRITE ( OUTF00D ) ;
             end (* tag/ca *) ;
       'E' : begin
               ASSIGNMEM ( OUTF00E , ADDR ( MEM ) , 8 ) ;
               REWRITE ( OUTF00E ) ;
             end (* tag/ca *) ;
       'F' : begin
               ASSIGNMEM ( OUTF00F , ADDR ( MEM ) , 8 ) ;
               REWRITE ( OUTF00F ) ;
             end (* tag/ca *) ;
     end (* case *) ;
   end (* ASSIGN_FILE *) ;



procedure CLOSE_FILE ;

   begin (* CLOSE_FILE *)
     case FN of
       '1' : CLOSE ( OUTF001 ) ;
       '2' : CLOSE ( OUTF002 ) ;
       '3' : CLOSE ( OUTF003 ) ;
       '4' : CLOSE ( OUTF004 ) ;
       '5' : CLOSE ( OUTF005 ) ;
       '6' : CLOSE ( OUTF006 ) ;
       '7' : CLOSE ( OUTF007 ) ;
       '8' : CLOSE ( OUTF008 ) ;
       '9' : CLOSE ( OUTF009 ) ;
       'A' : CLOSE ( OUTF00A ) ;
       'B' : CLOSE ( OUTF00B ) ;
       'C' : CLOSE ( OUTF00C ) ;
       'D' : CLOSE ( OUTF00D ) ;
       'E' : CLOSE ( OUTF00E ) ;
       'F' : CLOSE ( OUTF00F ) ;
     end (* case *) ;
   end (* CLOSE_FILE *) ;



procedure WRITEBUF ( var OUTF : TEXT ; HEXFLAG : CHAR ; CPOUT : CHARPTR
                   ; LOUT : INTEGER ) ;

   var I : INTEGER ;
       H : INTEGER ;
       HEX : INTEGER ;
       CH : CHAR ;
       CPLAST : CHARPTR ;
       OUTREC : CHAR ( 80 ) ;

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
         OUTREC := '' ;
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
             OUTREC [ I ] := CHR ( HEX ) ;
           end (* for *) ;
         WRITELN ( OUTF , OUTREC ) ;
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
             WRITE ( OUTF , CPOUT -> ) ;
             CPOUT := PTRADD ( CPOUT , 1 ) ;
           end (* for *) ;
         WRITELN ( OUTF ) ;
       end (* else *)
   end (* WRITEBUF *) ;



procedure WRITE_FILE ( HEXFLAG : CHAR ; CPOUT : CHARPTR ; LOUT :
                     INTEGER ) ;

   begin (* WRITE_FILE *)
     case FN of
       '1' : WRITEBUF ( OUTF001 , HEXFLAG , CPOUT , LOUT ) ;
       '2' : WRITEBUF ( OUTF002 , HEXFLAG , CPOUT , LOUT ) ;
       '3' : WRITEBUF ( OUTF003 , HEXFLAG , CPOUT , LOUT ) ;
       '4' : WRITEBUF ( OUTF004 , HEXFLAG , CPOUT , LOUT ) ;
       '5' : WRITEBUF ( OUTF005 , HEXFLAG , CPOUT , LOUT ) ;
       '6' : WRITEBUF ( OUTF006 , HEXFLAG , CPOUT , LOUT ) ;
       '7' : WRITEBUF ( OUTF007 , HEXFLAG , CPOUT , LOUT ) ;
       '8' : WRITEBUF ( OUTF008 , HEXFLAG , CPOUT , LOUT ) ;
       '9' : WRITEBUF ( OUTF009 , HEXFLAG , CPOUT , LOUT ) ;
       'A' : WRITEBUF ( OUTF00A , HEXFLAG , CPOUT , LOUT ) ;
       'B' : WRITEBUF ( OUTF00B , HEXFLAG , CPOUT , LOUT ) ;
       'C' : WRITEBUF ( OUTF00C , HEXFLAG , CPOUT , LOUT ) ;
       'D' : WRITEBUF ( OUTF00D , HEXFLAG , CPOUT , LOUT ) ;
       'E' : WRITEBUF ( OUTF00E , HEXFLAG , CPOUT , LOUT ) ;
       'F' : WRITEBUF ( OUTF00F , HEXFLAG , CPOUT , LOUT ) ;
     end (* case *) ;
   end (* WRITE_FILE *) ;



begin (* HAUPTPROGRAMM *)
  READLN ( ZINP ) ;
  while not EOF ( INPUT ) do
    begin
      MEMCPY ( ADDR ( TAG ) , ADDR ( ZINP ) , 6 ) ;

  (***********************************************************)
  (*   the record tagged with FILE contains the              *)
  (*   meta information (DSN, MEM, EXT, Hex Flag)            *)
  (***********************************************************)

      if TAG = '++FILE' then
        begin
          MEMCPY ( ADDR ( DSN ) , ADDR ( ZINP [ 8 ] ) , SIZEDSN ) ;
          MEMCPY ( ADDR ( MEM ) , ADDR ( ZINP [ 58 ] ) , SIZEMEM ) ;
          MEMCPY ( ADDR ( EXT ) , ADDR ( ZINP [ 71 ] ) , SIZEEXT ) ;
          HEXFLAG := ZINP [ 79 ] ;
          ASSIGN_FILE ( ADDR ( DSN ) , ADDR ( MEM ) , ADDR ( EXT ) ,
                        HEXFLAG ) ;
          LOUT := - 1 ;
          repeat
            if EOF ( INPUT ) then
              break ;
            READLN ( ZINP ) ;
            MEMCPY ( ADDR ( TAG ) , ADDR ( ZINP ) , 6 ) ;

  (***********************************************************)
  (*   the other records (DATA) are collected into           *)
  (*   the large buffer ZOUT. When the buffer is complete,   *)
  (*   WRITEBUF is called to flush the buffer.               *)
  (***********************************************************)

            if HEXFLAG = 'H' then
              begin
                if TAG = '++DATA' then
                  begin
                    if ZINP [ 7 ] = '1' then
                      begin
                        if LOUT > 0 then
                          WRITE_FILE ( HEXFLAG , ADDR ( ZOUT ) , LOUT )
                                       ;
                        LOUT := IVALSTR ( ADDR ( ZINP [ 8 ] ) , 5 ) ;
                        OUTPOS := IVALSTR ( ADDR ( ZINP [ 13 ] ) , 5 )
                                  ;
                        OUTPOS := OUTPOS * 2 ;
                        CPOUT := ADDR ( ZOUT ) ;
                        CPOUT := PTRADD ( CPOUT , OUTPOS ) ;
                        CPIN := ADDR ( ZINP [ 19 ] ) ;
                        MEMCPY ( CPOUT , CPIN , 60 ) ;
                      end (* then *)
                    else
                      begin
                        OUTPOS := IVALSTR ( ADDR ( ZINP [ 13 ] ) , 5 )
                                  ;
                        OUTPOS := OUTPOS * 2 ;
                        CPOUT := ADDR ( ZOUT ) ;
                        CPOUT := PTRADD ( CPOUT , OUTPOS ) ;
                        CPIN := ADDR ( ZINP [ 19 ] ) ;
                        MEMCPY ( CPOUT , CPIN , 60 ) ;
                      end (* else *)
                  end (* then *)
              end (* then *)
            else
              if TAG <> '++FILE' then
                WRITE_FILE ( HEXFLAG , ADDR ( ZINP ) , 80 ) ;
          until TAG = '++FILE' ;
          if LOUT > 0 then
            WRITE_FILE ( HEXFLAG , ADDR ( ZOUT ) , LOUT ) ;
          CLOSE_FILE ;
        end (* then *)
      else
        begin
          WRITELN ( '+++ falsche Satzart: ' , TAG ) ;
          break ;
        end (* else *)
    end (* while *)
end (* HAUPTPROGRAMM *) .
