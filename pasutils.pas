module $PASUTIL ;


type PLATFORM = ( PLATF_UNKNOWN , PLATF_INTEL , PLATF_MAINFRAME ) ;
     CHAR8 = array [ 1 .. 8 ] of CHAR ;
     CHAR10 = array [ 1 .. 10 ] of CHAR ;
     CHAR80 = array [ 1 .. 80 ] of CHAR ;
     CHARPTR = -> CHAR ;


const KLEINBUCHST : set of CHAR =
      [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' ] ;


static PLATF : PLATFORM ;



function $PASSYS ( FUNCCODE : INTEGER ; X : VOIDPTR ) : VOIDPTR ;

(****************************************************)
(*  ist in PASMONN.ASS implementiert und            *)
(*  realisiert Basis-Storage-Dienste wie            *)
(*  GETMAIN und FREEMAIN                            *)
(****************************************************)


   EXTERNAL ;



procedure CMSX ( CMD : CHARPTR ; var RETCODE : INTEGER ) ;

   EXTERNAL ;



local procedure CHECK_PLATFORM ;

   begin (* CHECK_PLATFORM *)
     if ORD ( '0' ) = 0X30 then
       PLATF := PLATF_INTEL
     else
       PLATF := PLATF_MAINFRAME
   end (* CHECK_PLATFORM *) ;



procedure HALT ;

   begin (* HALT *)
     EXIT ( 8 ) ;
   end (* HALT *) ;



procedure DATETIME ( var DAT : CHAR8 ; var TIM : CHAR8 ) ;

   var DATX : CHAR10 ;

   begin (* DATETIME *)
     DATX := DATE ;
     DAT := 'MM/DD/YY' ;
     DAT [ 1 ] := DATX [ 4 ] ;
     DAT [ 2 ] := DATX [ 5 ] ;
     DAT [ 4 ] := DATX [ 1 ] ;
     DAT [ 5 ] := DATX [ 2 ] ;
     DAT [ 7 ] := DATX [ 9 ] ;
     DAT [ 8 ] := DATX [ 10 ] ;
     PACK ( TIME , 1 , TIM ) ;
   end (* DATETIME *) ;



procedure DATTIM10 ( var DAT : CHAR10 ; var TIM : CHAR10 ) ;

   var DATX : CHAR10 ;

   begin (* DATTIM10 *)
     DATX := DATE ;
     DAT := 'DD.MM.YYYY' ;
     DAT [ 1 ] := DATX [ 4 ] ;
     DAT [ 2 ] := DATX [ 5 ] ;
     DAT [ 4 ] := DATX [ 1 ] ;
     DAT [ 5 ] := DATX [ 2 ] ;
     DAT [ 7 ] := DATX [ 7 ] ;
     DAT [ 8 ] := DATX [ 8 ] ;
     DAT [ 9 ] := DATX [ 9 ] ;
     DAT [ 10 ] := DATX [ 10 ] ;
     TIM := TIME ;
   end (* DATTIM10 *) ;



procedure TERMIN ( var X : TEXT ) ;

   var FCB : VOIDPTR ;
       PDDN : -> CHAR8 ;
       PTERM : -> CHAR ;
       CMSCMD : CHAR80 ;
       CPT : -> CHAR ;
       RC : INTEGER ;

   begin (* TERMIN *)
     if PLATF = PLATF_UNKNOWN then
       CHECK_PLATFORM ;
     if PLATF = PLATF_INTEL then
       begin
         FCB := FILEFCB ( X ) ;
         PDDN := PTRADD ( FCB , 8 ) ;
         PTERM := PTRADD ( FCB , 279 ) ;
         PTERM -> := 'Y'
       end (* then *)
     else
       begin
         FCB := FILEFCB ( X ) ;
         CMSCMD := 'FILEDEF XXXXXXXX CLEAR #' ;
         CPT := PTRADD ( ADDR ( CMSCMD ) , 8 ) ;
         MEMCPY ( CPT , FCB , 8 ) ;
         CMSX ( ADDR ( CMSCMD ) , RC ) ;
         CMSCMD := 'FILEDEF XXXXXXXX TERM (RECFM V LRECL 255 #' ;
         MEMCPY ( CPT , FCB , 8 ) ;
         CMSX ( ADDR ( CMSCMD ) , RC ) ;
         PTERM := PTRADD ( FCB , 36 ) ;
         PTERM -> := 'Y'
       end (* else *)
   end (* TERMIN *) ;



procedure TERMOUT ( var X : TEXT ) ;

   var FCB : VOIDPTR ;
       PDDN : -> CHAR8 ;
       PTERM : -> CHAR ;
       CMSCMD : CHAR80 ;
       CPT : -> CHAR ;
       RC : INTEGER ;

   begin (* TERMOUT *)
     if PLATF = PLATF_UNKNOWN then
       CHECK_PLATFORM ;
     if PLATF = PLATF_INTEL then
       begin
         FCB := FILEFCB ( X ) ;
         PDDN := PTRADD ( FCB , 8 ) ;
         PTERM := PTRADD ( FCB , 279 ) ;
         PTERM -> := 'Y'
       end (* then *)
     else
       begin
         FCB := FILEFCB ( X ) ;
         CMSCMD := 'FILEDEF XXXXXXXX CLEAR #' ;
         CPT := PTRADD ( ADDR ( CMSCMD ) , 8 ) ;
         MEMCPY ( CPT , FCB , 8 ) ;
         CMSX ( ADDR ( CMSCMD ) , RC ) ;
         CMSCMD := 'FILEDEF XXXXXXXX TERM (RECFM V LRECL 255 #' ;
         MEMCPY ( CPT , FCB , 8 ) ;
         CMSX ( ADDR ( CMSCMD ) , RC ) ;
         PTERM := PTRADD ( FCB , 36 ) ;
         PTERM -> := 'Y'
       end (* else *)
   end (* TERMOUT *) ;



procedure ASSIGN ( var X : ANYFILE ; FNAME : CHARPTR ; LEN : INTEGER )
                 ;

   var FCB : VOIDPTR ;
       PDDN : -> CHAR8 ;
       PFN : -> CHAR ;
       CMSCMD : CHAR80 ;
       CPT : -> CHAR ;
       RC : INTEGER ;

   begin (* ASSIGN *)
     if PLATF = PLATF_UNKNOWN then
       CHECK_PLATFORM ;
     if PLATF = PLATF_INTEL then
       begin
         FCB := FILEFCB ( X ) ;
         PDDN := PTRADD ( FCB , 8 ) ;
         PFN := PTRADD ( FCB , 17 ) ;
         MEMSET ( PFN , CHR ( 0 ) , 257 ) ;
         MEMCPY ( PFN , FNAME , LEN ) ;
       end (* then *)
     else
       begin
         FCB := FILEFCB ( X ) ;
         CMSCMD := 'FILEDEF XXXXXXXX CLEAR #' ;
         CPT := PTRADD ( ADDR ( CMSCMD ) , 8 ) ;
         MEMCPY ( CPT , FCB , 8 ) ;
         CMSX ( ADDR ( CMSCMD ) , RC ) ;
         CMSCMD := 'FILEDEF XXXXXXXX DISK' ;
         MEMCPY ( CPT , FCB , 8 ) ;
         CPT := PTRADD ( ADDR ( CMSCMD ) , 22 ) ;
         if 22 + LEN > 80 then
           LEN := 80 - 22 ;
         MEMCPY ( CPT , FNAME , LEN ) ;
         CMSX ( ADDR ( CMSCMD ) , RC ) ;
       end (* else *)
   end (* ASSIGN *) ;



procedure ASSIGNMEM ( var X : ANYFILE ; MEMBNAME : CHARPTR ; LEN :
                    INTEGER ) ;

   var FCB : VOIDPTR ;
       CPT : -> CHAR ;

   begin (* ASSIGNMEM *)
     if PLATF = PLATF_UNKNOWN then
       CHECK_PLATFORM ;
     if PLATF = PLATF_INTEL then
       begin
         
       end (* then *)
     else
       begin
         FCB := FILEFCB ( X ) ;
         CPT := PTRADD ( FCB , 40 ) ;
         MEMSET ( CPT , ' ' , 8 ) ;
         if MEMBNAME <> NIL then
           if LEN > 8 then
             MEMCPY ( CPT , MEMBNAME , 8 )
           else
             MEMCPY ( CPT , MEMBNAME , LEN )
       end (* else *)
   end (* ASSIGNMEM *) ;



procedure CLRSCRN ;

(**************************************)
(*   LOESCHT DEN BILDSCHIRM MITTELS   *)
(*   CMS-KOMMANDO CLRSCRN.            *)
(**************************************)


   var RC : INTEGER ;
       CMD : array [ 1 .. 80 ] of CHAR ;
       X : VOIDPTR ;

   begin (* CLRSCRN *)
     if ORD ( '0' ) <> 0X30 then
       begin
         X := $PASSYS ( 13 , NIL ) ;
         CMD := 'CLRSCRN #' ;
         CMSX ( ADDR ( CMD ) , RC ) ;
       end (* then *)
     else
       begin
         CMD := 'CLS #' ;
         WINX ( ADDR ( CMD ) , RC ) ;
       end (* else *)
   end (* CLRSCRN *) ;



procedure MOVEPARM ( X : VOIDPTR ; LEN : INTEGER ) ;

(**************************************)
(*   uebertraegt parm in angegeb.     *)
(*   string (ohne blanks)             *)
(**************************************)


   var CP : -> CHAR ;
       SX : INTEGER ;

   begin (* MOVEPARM *)
     if OSPARM <> NIL then
       with OSPARM -> do
         begin
           SX := 1 ;
           CP := X ;
           while SX <= PLENGTH do
             begin
               if PSTRING [ SX ] = ' ' then
                 begin
                   SX := SX + 1 ;
                   continue ;
                 end (* then *) ;
               CP -> := PSTRING [ SX ] ;
               SX := SX + 1 ;
               CP := PTRADD ( CP , 1 ) ;
               if PTRDIFF ( CP , X ) >= LEN then
                 break ;
             end (* while *)
         end (* with *) ;
   end (* MOVEPARM *) ;



function TOUPPER ( CH : CHAR ) : CHAR ;

(******************************************)
(*   SETZT KLEINBUCHSTABEN IN GROSSE UM   *)
(******************************************)


   begin (* TOUPPER *)
     if CH in KLEINBUCHST then
       TOUPPER := CHR ( ORD ( CH ) - ORD ( 'a' ) + ORD ( 'A' ) )
     else
       TOUPPER := CH
   end (* TOUPPER *) ;



procedure TOUPPERS ( X : VOIDPTR ; LEN : INTEGER ) ;

(******************************************)
(*   SETZT KLEINBUCHSTABEN IN GROSSE UM   *)
(*   fuer strings                         *)
(******************************************)


   var CP : -> CHAR ;

   begin (* TOUPPERS *)
     CP := X ;
     while PTRDIFF ( CP , X ) < LEN do
       begin
         CP -> := TOUPPER ( CP -> ) ;
         CP := PTRADD ( CP , 1 ) ;
       end (* while *)
   end (* TOUPPERS *) ;



procedure DUMPSTOR ( PVON : VOIDPTR ; PBIS : VOIDPTR ) ;

(*********************************************************)
(*  Speicherbereich von PVON bis PBIS hexadezimal        *)
(*  ausgeben                                             *)
(*********************************************************)


   var P1 : VOIDPTR ;
       P2 : VOIDPTR ;
       MOD1 : INTEGER ;
       MOD2 : INTEGER ;


   procedure DUMPCHAR ( CH : CHAR ) ;

      const DUMPSHOWCHARS : set of CHAR =
            [ 'a' .. 'i' , 'j' .. 'r' , 's' .. 'z' , 'A' .. 'I' , 'J'
              .. 'R' , 'S' .. 'Z' , '0' .. '9' , ' ' , ',' , '.' , '-'
              , ';' , ':' , '_' , '!' , '"' , '$' , '%' , '&' , '/' ,
              '(' , ')' , '=' , '?' , '+' , '*' , '#' , '*' ] ;

      begin (* DUMPCHAR *)
        if CH in DUMPSHOWCHARS then
          WRITE ( CH )
        else
          WRITE ( '.' )
      end (* DUMPCHAR *) ;


   procedure DUMPZEILE ( ADR : VOIDPTR ; P1 : VOIDPTR ; P2 : VOIDPTR )
                       ;

      var CH : -> CHAR ;
          I : INTEGER ;
          J : INTEGER ;

      const HEXTAB : array [ 0 .. 15 ] of CHAR = '0123456789abcdef' ;

      begin (* DUMPZEILE *)
        WRITE ( ADR , ': ' ) ;
        CH := ADR ;
        for J := 1 to 4 do
          begin
            for I := 1 to 4 do
              begin
                if ( PTRDIFF ( CH , P1 ) < 0 ) or ( PTRDIFF ( CH , P2 )
                > 0 ) then
                  WRITE ( '..' )
                else
                  WRITE ( HEXTAB [ ORD ( CH -> ) DIV 16 ] , HEXTAB [
                          ORD ( CH -> ) MOD 16 ] ) ;
                CH := PTRADD ( CH , 1 ) ;
              end (* for *) ;
            WRITE ( ' ' ) ;
          end (* for *) ;
        CH := ADR ;
        WRITE ( ' *' ) ;
        for I := 1 to 16 do
          begin
            if ( PTRDIFF ( CH , P1 ) < 0 ) or ( PTRDIFF ( CH , P2 ) > 0
            ) then
              WRITE ( ' ' )
            else
              DUMPCHAR ( CH -> ) ;
            CH := PTRADD ( CH , 1 )
          end (* for *) ;
        WRITELN ( '*' ) ;
      end (* DUMPZEILE *) ;


   begin (* DUMPSTOR *)
     P1 := PTRADD ( PVON , - 16 ) ;
     MOD1 := PTR2INT ( P1 ) MOD 16 ;
     P1 := PTRADD ( P1 , 16 - MOD1 ) ;
     P2 := PTRADD ( PBIS , 16 ) ;
     MOD2 := PTR2INT ( P2 ) MOD 16 ;
     P2 := PTRADD ( P2 , - MOD2 ) ;
     while PTRDIFF ( P1 , P2 ) < 0 do
       begin
         DUMPZEILE ( P1 , PVON , PBIS ) ;
         P1 := PTRADD ( P1 , 16 ) ;
       end (* while *) ;
   end (* DUMPSTOR *) ;



procedure INTTOSTR ( CP : VOIDPTR ; LEN : INTEGER ; VAL : INTEGER ;
                   ZEROES : BOOLEAN ) ;

   var BUFFER : array [ 1 .. 20 ] of CHAR ;
       MINUS : BOOLEAN ;
       LETZT : INTEGER ;
       I : INTEGER ;
       LIMIT : INTEGER ;
       LENX : INTEGER ;
       POSX : INTEGER ;

   begin (* INTTOSTR *)
     if VAL < 0 then
       begin
         VAL := - VAL ;
         MINUS := TRUE
       end (* then *)
     else
       MINUS := FALSE ;
     I := 20 ;
     BUFFER := ' ' ;
     while VAL > 0 do
       begin
         LETZT := VAL MOD 10 ;
         BUFFER [ I ] := CHR ( ORD ( '0' ) + LETZT ) ;
         I := I - 1 ;
         VAL := VAL DIV 10 ;
       end (* while *) ;
     LIMIT := 20 - LEN ;
     if ZEROES then
       while I > LIMIT do
         begin
           BUFFER [ I ] := '0' ;
           I := I - 1 ;
         end (* while *) ;
     if MINUS then
       begin
         if ZEROES then
           I := I + 1 ;
         BUFFER [ I ] := '-' ;
         I := I - 1 ;
       end (* then *) ;
     LENX := 20 - I ;
     POSX := LEN - LENX ;
     if POSX < 0 then
       begin
         I := I - POSX ;
         POSX := 0 ;
       end (* then *) ;
     MEMSET ( CP , ' ' , LEN ) ;
     MEMCPY ( PTRADD ( CP , POSX ) , ADDR ( BUFFER [ I + 1 ] ) , LENX )
              ;
   end (* INTTOSTR *) ;



procedure WRITEPTR_LEN ( var F : TEXT ; CPSTART : VOIDPTR ; LEN :
                       INTEGER ; TRIM_LEFT : BOOLEAN ; TRIM_RIGHT :
                       BOOLEAN ) ;

   var CP : -> CHAR ;
       I : INTEGER ;
       TRIGGER : BOOLEAN ;

   begin (* WRITEPTR_LEN *)
     CP := CPSTART ;
     if TRIM_RIGHT then
       begin
         CP := PTRADD ( CPSTART , LEN - 1 ) ;
         while PTRDIFF ( CP , CPSTART ) >= 0 do
           begin
             if CP -> <> ' ' then
               break ;
             CP := PTRADD ( CP , - 1 ) ;
           end (* while *) ;
         CP := PTRADD ( CP , 1 ) ;
         LEN := PTRDIFF ( CP , CPSTART ) ;
       end (* then *) ;
     CP := CPSTART ;
     TRIGGER := FALSE ;
     for I := 1 to LEN do
       begin
         if TRIM_LEFT then
           begin
             if CP -> <> ' ' then
               TRIGGER := TRUE ;
             if TRIGGER then
               WRITE ( F , CP -> ) ;
           end (* then *)
         else
           WRITE ( F , CP -> ) ;
         CP := PTRADD ( CP , 1 ) ;
       end (* for *) ;
   end (* WRITEPTR_LEN *) ;



function IVALSTR ( CP : CHARPTR ; LEN : INTEGER ) : INTEGER ;

   var X : INTEGER ;
       STATUS : INTEGER ;
       MINUS : BOOLEAN ;

   begin (* IVALSTR *)
     X := 0 ;
     STATUS := 1 ;
     MINUS := FALSE ;
     while TRUE do
       begin
         if LEN <= 0 then
           break ;
         case STATUS of
           1 : case CP -> of
                 ' ' : begin
                         CP := PTRADD ( CP , 1 ) ;
                         LEN := LEN - 1 ;
                       end (* tag/ca *) ;
                 '+' , '-' :
                   begin
                     MINUS := ( CP -> = '-' ) ;
                     CP := PTRADD ( CP , 1 ) ;
                     LEN := LEN - 1 ;
                     STATUS := 2 ;
                   end (* tag/ca *) ;
                 '0' .. '9' :
                   begin
                     X := ORD ( CP -> ) - ORD ( '0' ) ;
                     CP := PTRADD ( CP , 1 ) ;
                     LEN := LEN - 1 ;
                     STATUS := 2 ;
                   end (* tag/ca *) ;
                 otherwise
                   break
               end (* case *) ;
           2 : case CP -> of
                 '0' .. '9' :
                   begin
                     X := X * 10 + ORD ( CP -> ) - ORD ( '0' ) ;
                     CP := PTRADD ( CP , 1 ) ;
                     LEN := LEN - 1 ;
                   end (* tag/ca *) ;
                 otherwise
                   break
               end (* case *)
         end (* case *)
       end (* while *) ;
     if MINUS then
       X := - X ;
     IVALSTR := X ;
   end (* IVALSTR *) ;



function DVALSTR ( CP : CHARPTR ; LEN : INTEGER ) : REAL ;

   var X : REAL ;
       SCALE : REAL ;
       STATUS : INTEGER ;
       MINUS : BOOLEAN ;
       COMMA_FOUND : BOOLEAN ;

   begin (* DVALSTR *)
     X := 0 ;
     SCALE := 1.0 ;
     STATUS := 1 ;
     MINUS := FALSE ;
     COMMA_FOUND := FALSE ;
     while TRUE do
       begin
         if LEN <= 0 then
           break ;
         case STATUS of
           1 : case CP -> of
                 ' ' : begin
                         CP := PTRADD ( CP , 1 ) ;
                         LEN := LEN - 1 ;
                       end (* tag/ca *) ;
                 '+' , '-' :
                   begin
                     MINUS := ( CP -> = '-' ) ;
                     CP := PTRADD ( CP , 1 ) ;
                     LEN := LEN - 1 ;
                     STATUS := 2 ;
                   end (* tag/ca *) ;
                 ',' , '.' :
                   begin
                     COMMA_FOUND := TRUE ;
                     CP := PTRADD ( CP , 1 ) ;
                     LEN := LEN - 1 ;
                     STATUS := 2 ;
                   end (* tag/ca *) ;
                 '0' .. '9' :
                   begin
                     X := ORD ( CP -> ) - ORD ( '0' ) ;
                     CP := PTRADD ( CP , 1 ) ;
                     LEN := LEN - 1 ;
                     STATUS := 2 ;
                   end (* tag/ca *) ;
                 otherwise
                   break
               end (* case *) ;
           2 : case CP -> of
                 '0' .. '9' :
                   begin
                     X := X * 10 + ORD ( CP -> ) - ORD ( '0' ) ;
                     if COMMA_FOUND then
                       SCALE := SCALE * 10.0 ;
                     CP := PTRADD ( CP , 1 ) ;
                     LEN := LEN - 1 ;
                   end (* tag/ca *) ;
                 ',' , '.' :
                   begin
                     COMMA_FOUND := TRUE ;
                     CP := PTRADD ( CP , 1 ) ;
                     LEN := LEN - 1 ;
                   end (* tag/ca *) ;
                 otherwise
                   break
               end (* case *)
         end (* case *)
       end (* while *) ;
     if MINUS then
       X := - X ;
     if COMMA_FOUND then
       X := X / SCALE ;
     DVALSTR := X ;
   end (* DVALSTR *) ;



procedure READSYMB ( var F : TEXT ; X : VOIDPTR ; LEN : INTEGER ) ;

   var CP : CHARPTR ;

   begin (* READSYMB *)
     CP := X ;
     MEMSET ( CP , ' ' , LEN ) ;
     while ( F -> <> ' ' ) do
       begin
         if PTRDIFF ( CP , X ) >= LEN then
           break ;
         CP -> := F -> ;
         GET ( F ) ;
         CP := PTRADD ( CP , 1 ) ;
       end (* while *) ;
   end (* READSYMB *) ;



begin (* HAUPTPROGRAMM *)
  
end (* HAUPTPROGRAMM *) .
