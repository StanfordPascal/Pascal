program DOWNLOAD ( CONTROL , OUTPUT , PDS001 , PDS002 , PDS003 , PDS004
                   , PDS005 , PDS006 , PDS007 , PDS008 , PDS009 ,
                   PDS00A , PDS00B , PDS00C , PDS00D , PDS00E , PDS00F
                   ) ;

(***********************************************************)
(*                                                         *)
(*   This program copies many pds members into one         *)
(*   large file, which is the base of the distribution     *)
(*   of the Stanford Pascal compiler - MVS edition         *)
(*                                                         *)
(*   Most files are text files, but some are binary;       *)
(*   the binary files are hex encoded, so that this        *)
(*   transfer file is a pure text file.                    *)
(*                                                         *)
(*   It contains all the needed meta information, so       *)
(*   that on Windows etc. the files can be extracted       *)
(*   and distributed to certain directories. On the        *)
(*   target mainframes, the original members can be        *)
(*   restored                                              *)
(*                                                         *)
(***********************************************************)
(*                                                         *)
(*   Author: Bernd Oppolzer - May 2017                     *)
(*                                                         *)
(***********************************************************)
(*                                                         *)
(*   The input file CONTROL tells the member names         *)
(*   of the input files                                    *)
(*                                                         *)
(*   Syntax example:                                       *)
(*                                                         *)
(*   8 PASCALN.COMPILER.TEXT        PASCAL1    OBJ    H    *)
(*                                                         *)
(*   8 - number of input file (PDS008 in this case)        *)
(*   2nd parm - DSNAME, which is written to the transfer   *)
(*              file; take care, that this name matches    *)
(*              the allocation of the file number !!       *)
(*   3rd parm - member name, which is read from the        *)
(*              input file (PDS008 in this case)           *)
(*   4th parm - file type for target system (e.g. Win)     *)
(*   5th parm - H if hex encoding needed, otherwise        *)
(*              omit this parameter                        *)
(*                                                         *)
(*   further example:                                      *)
(*                                                         *)
(*   7 PASCALN.TESTPGM.CNTL         TESTSCAL   JOB         *)
(*   7 PASCALN.TESTPGM.CNTL         TESTSTAT   JOB         *)
(*   7 PASCALN.TESTPGM.CNTL         XCOMP      JOB         *)
(*   **************************************************    *)
(*   8 PASCALN.COMPILER.TEXT        PASCAL1    OBJ    H    *)
(*   8 PASCALN.COMPILER.TEXT        PASCAL2    OBJ    H    *)
(*                                                         *)
(***********************************************************)



const SIZEDSN = 44 ;
      SIZEMEM = 8 ;
      SIZEEXT = 3 ;
      MAXLINEOFFS = 72 ;
      MAXOUT = 100 ;


type CHARPTR = -> CHAR ;


var CONTROL : TEXT ;
    PDS001 : TEXT ;
    PDS002 : TEXT ;
    PDS003 : TEXT ;
    PDS004 : TEXT ;
    PDS005 : TEXT ;
    PDS006 : TEXT ;
    PDS007 : TEXT ;
    PDS008 : TEXT ;
    PDS009 : TEXT ;
    PDS00A : TEXT ;
    PDS00B : TEXT ;
    PDS00C : TEXT ;
    PDS00D : TEXT ;
    PDS00E : TEXT ;
    PDS00F : TEXT ;
    LINEBUF : array [ 1 .. 80 ] of CHAR ;
    X : INTEGER ;
    XFOUND : INTEGER ;
    FILENO : CHAR ;
    PDSN : CHARPTR ;
    PMEM : CHARPTR ;
    PEXT : CHARPTR ;
    PEND : CHARPTR ;
    LEN : INTEGER ;
    DSN : array [ 1 .. SIZEDSN ] of CHAR ;
    MEM : array [ 1 .. SIZEMEM ] of CHAR ;
    EXT : array [ 1 .. SIZEEXT ] of CHAR ;
    HEXFLAG : CHAR ;



procedure WRITEHEX ( C : CHAR ) ;

   var C1 : CHAR ;
       C2 : CHAR ;

   const HEX : array [ 0 .. 15 ] of CHAR = '0123456789ABCDEF' ;

   begin (* WRITEHEX *)
     C1 := HEX [ ORD ( C ) DIV 16 ] ;
     C2 := HEX [ ORD ( C ) MOD 16 ] ;
     WRITE ( C1 , C2 ) ;
   end (* WRITEHEX *) ;



procedure COPYLINE ( var F : TEXT ; HEXFLAG : CHAR ) ;

(*********************************************)
(*   read line into large buffer             *)
(*   store length and line info              *)
(*   into transfer file                      *)
(*   convert to hex if required              *)
(*********************************************)


   var LINEBUF : array [ 1 .. 1000 ] of CHAR ;
       I : INTEGER ;
       CNT : INTEGER ;
       CH : CHAR ;
       START : INTEGER ;
       TAG : CHAR ;
       CNTOUT : INTEGER ;

   begin (* COPYLINE *)
     I := 0 ;

     /*********************************************/
     /*   read line and remember char cnt         */
     /*********************************************/

     while TRUE do
       begin
         READ ( F , CH ) ;
         I := I + 1 ;
         LINEBUF [ I ] := CH ;
         if EOLN ( F ) then
           break ;
       end (* while *) ;
     READLN ( F ) ;
     CNT := I ;

     /*********************************************/
     /*   write line, max 100 chars (maxout)      */
     /*   per output line                         */
     /*   format:                                 */
     /*   - DATAx (x = seqno of line part)        */
     /*   - complete line length                  */
     /*   - offset of line part                   */
     /*   - line part content (char or hex)       */
     /*********************************************/

     if HEXFLAG = 'H' then
       begin
         CNTOUT := 30 ;
         START := 1 ;
         TAG := '1' ;
         while TRUE do
           begin
             WRITE ( '++DATA' , TAG , CNT : 5 , START - 1 : 5 , ' ' ) ;
             for I := START to START + CNTOUT - 1 do
               if I <= CNT then
                 WRITEHEX ( LINEBUF [ I ] )
               else
                 break ;
             WRITELN ;
             START := START + CNTOUT ;
             if START > CNT then
               break ;
             TAG := CHR ( ORD ( TAG ) + 1 ) ;
           end (* while *) ;
       end (* then *)
     else
       begin
         for I := 1 to CNT do
           WRITE ( LINEBUF [ I ] ) ;
         WRITELN
       end (* else *)
   end (* COPYLINE *) ;



procedure WRITE_FILE ( var F : TEXT ; PDSN : CHARPTR ; PMEMB : CHARPTR
                     ; PEXT : CHARPTR ; HEXFLAG : CHAR ) ;

   var P : VOIDPTR ;
       CP : -> CHAR ;
       DSNAME : array [ 1 .. SIZEDSN ] of CHAR ;
       MEMBNAME : array [ 1 .. SIZEMEM ] of CHAR ;
       EXT : array [ 1 .. SIZEEXT ] of CHAR ;

   begin (* WRITE_FILE *)
     MEMCPY ( ADDR ( DSNAME ) , PDSN , SIZEDSN ) ;
     MEMCPY ( ADDR ( MEMBNAME ) , PMEMB , SIZEMEM ) ;
     MEMCPY ( ADDR ( EXT ) , PEXT , SIZEEXT ) ;

     /*********************************************/
     /*   assign member name to input file        */
     /*********************************************/

     ASSIGNMEM ( F , ADDR ( MEMBNAME ) , SIZEMEM ) ;

     /*********************************************/
     /*   try reset and check success             */
     /*********************************************/

     RESET ( F ) ;
     P := FILEFCB ( F ) ;
     CP := PTRADD ( P , 32 ) ;
     if CP -> = '0' then
       return ;

     /*********************************************/
     /*   write header info into transfer file    */
     /*********************************************/

     WRITELN ( '++FILE ' , DSNAME , ' MEMB ' , MEMBNAME , ' EXT ' , EXT
               , ' HEX ' , HEXFLAG ) ;

     /*********************************************/
     /*   copy lines until eof of input member    */
     /*********************************************/

     while not EOF ( F ) do
       begin
         COPYLINE ( F , HEXFLAG ) ;
       end (* while *) ;

     /*********************************************/
     /*   close input file (must be closed        */
     /*   before new assignmem)                   */
     /*********************************************/

     CLOSE ( F ) ;
   end (* WRITE_FILE *) ;



begin (* HAUPTPROGRAMM *)
  RESET ( CONTROL ) ;
  while not EOF ( CONTROL ) do
    begin

  /*********************************************/
  /*   read line from control file             */
  /*********************************************/

      READLN ( CONTROL , LINEBUF ) ;
      if LINEBUF [ 1 ] = '*' then
        continue ;

  /*********************************************/
  /*   no comment line, look for file no       */
  /*********************************************/

      XFOUND := 0 ;
      for X := 1 to 80 do
        if LINEBUF [ X ] <> ' ' then
          begin
            XFOUND := X ;
            break
          end (* then *) ;
      if XFOUND = 0 then
        continue ;
      FILENO := LINEBUF [ XFOUND ] ;

  /*********************************************/
  /*   extract ds name from linebuf            */
  /*********************************************/

      X := XFOUND + 1 ;
      while ( X < MAXLINEOFFS ) and ( LINEBUF [ X ] = ' ' ) do
        X := X + 1 ;
      PDSN := ADDR ( LINEBUF [ X ] ) ;
      while ( X < MAXLINEOFFS ) and ( LINEBUF [ X ] <> ' ' ) do
        X := X + 1 ;
      PEND := ADDR ( LINEBUF [ X ] ) ;
      LEN := PTRDIFF ( PEND , PDSN ) ;
      if LEN > SIZEDSN then
        LEN := SIZEDSN ;
      DSN := ' ' ;
      MEMCPY ( ADDR ( DSN ) , PDSN , LEN ) ;

  /*********************************************/
  /*   extract member name from linebuf        */
  /*********************************************/

      while ( X < MAXLINEOFFS ) and ( LINEBUF [ X ] = ' ' ) do
        X := X + 1 ;
      PMEM := ADDR ( LINEBUF [ X ] ) ;
      while ( X < MAXLINEOFFS ) and ( LINEBUF [ X ] <> ' ' ) do
        X := X + 1 ;
      PEND := ADDR ( LINEBUF [ X ] ) ;
      LEN := PTRDIFF ( PEND , PMEM ) ;
      if LEN > SIZEMEM then
        LEN := SIZEMEM ;
      MEM := ' ' ;
      MEMCPY ( ADDR ( MEM ) , PMEM , LEN ) ;

  /*********************************************/
  /*   extract extension name from linebuf     */
  /*********************************************/

      while ( X < MAXLINEOFFS ) and ( LINEBUF [ X ] = ' ' ) do
        X := X + 1 ;
      PEXT := ADDR ( LINEBUF [ X ] ) ;
      while ( X < MAXLINEOFFS ) and ( LINEBUF [ X ] <> ' ' ) do
        X := X + 1 ;
      PEND := ADDR ( LINEBUF [ X ] ) ;
      LEN := PTRDIFF ( PEND , PEXT ) ;
      if LEN > SIZEEXT then
        LEN := SIZEEXT ;
      EXT := ' ' ;
      MEMCPY ( ADDR ( EXT ) , PEXT , LEN ) ;

  /*********************************************/
  /*   look for hex flag                       */
  /*********************************************/

      while ( X < MAXLINEOFFS ) and ( LINEBUF [ X ] = ' ' ) do
        X := X + 1 ;
      HEXFLAG := LINEBUF [ X ] ;
      if HEXFLAG <> 'H' then
        HEXFLAG := '-' ;

  /**************************************************/
  /*   call write function depending on fileno      */
  /**************************************************/

      case FILENO of
        '1' : WRITE_FILE ( PDS001 , ADDR ( DSN ) , ADDR ( MEM ) , ADDR
                           ( EXT ) , HEXFLAG ) ;
        '2' : WRITE_FILE ( PDS002 , ADDR ( DSN ) , ADDR ( MEM ) , ADDR
                           ( EXT ) , HEXFLAG ) ;
        '3' : WRITE_FILE ( PDS003 , ADDR ( DSN ) , ADDR ( MEM ) , ADDR
                           ( EXT ) , HEXFLAG ) ;
        '4' : WRITE_FILE ( PDS004 , ADDR ( DSN ) , ADDR ( MEM ) , ADDR
                           ( EXT ) , HEXFLAG ) ;
        '5' : WRITE_FILE ( PDS005 , ADDR ( DSN ) , ADDR ( MEM ) , ADDR
                           ( EXT ) , HEXFLAG ) ;
        '6' : WRITE_FILE ( PDS006 , ADDR ( DSN ) , ADDR ( MEM ) , ADDR
                           ( EXT ) , HEXFLAG ) ;
        '7' : WRITE_FILE ( PDS007 , ADDR ( DSN ) , ADDR ( MEM ) , ADDR
                           ( EXT ) , HEXFLAG ) ;
        '8' : WRITE_FILE ( PDS008 , ADDR ( DSN ) , ADDR ( MEM ) , ADDR
                           ( EXT ) , HEXFLAG ) ;
        '9' : WRITE_FILE ( PDS009 , ADDR ( DSN ) , ADDR ( MEM ) , ADDR
                           ( EXT ) , HEXFLAG ) ;
        'A' : WRITE_FILE ( PDS00A , ADDR ( DSN ) , ADDR ( MEM ) , ADDR
                           ( EXT ) , HEXFLAG ) ;
        'B' : WRITE_FILE ( PDS00B , ADDR ( DSN ) , ADDR ( MEM ) , ADDR
                           ( EXT ) , HEXFLAG ) ;
        'C' : WRITE_FILE ( PDS00C , ADDR ( DSN ) , ADDR ( MEM ) , ADDR
                           ( EXT ) , HEXFLAG ) ;
        'D' : WRITE_FILE ( PDS00D , ADDR ( DSN ) , ADDR ( MEM ) , ADDR
                           ( EXT ) , HEXFLAG ) ;
        'E' : WRITE_FILE ( PDS00E , ADDR ( DSN ) , ADDR ( MEM ) , ADDR
                           ( EXT ) , HEXFLAG ) ;
        'F' : WRITE_FILE ( PDS00F , ADDR ( DSN ) , ADDR ( MEM ) , ADDR
                           ( EXT ) , HEXFLAG ) ;
      end (* case *) ;
    end (* while *)
end (* HAUPTPROGRAMM *) .
