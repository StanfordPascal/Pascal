program TEST ;


type SCAN_BLOCK = record
                    MODUS : INTEGER ;        // modus of scanner
                    DATEIENDE : INTEGER ;    // end of file indicator
                    ENDOFLINE : BOOLEAN ;    // end of line indicator
                    SLINE : SOURCELINE ;     // stored source line
                    LINENR : INTEGER ;       // line number of symbol
                    LINEPOS : INTEGER ;      // line position of symb
                    LINELEN : INTEGER ;      // line length
                    LOOKAHEAD : CHAR ;       // lookahead character
                    SYMBOLNR : SYMB ;        // symbol read
                    SYMBOL : SOURCELINE ;    // characters of symb
                    LSYMBOL : INTEGER ;      // no of chars in symb
                    MAXLSYMBOL : INTEGER ;   //
                    UFZAHL : INTEGER ;       // no of undef errors
                    SFZAHL : INTEGER ;       // no of severe errors
                    FEZAHL : INTEGER ;       // no of errors
                    WAZAHL : INTEGER ;       // no of warnings
                    INZAHL : INTEGER ;       // no of informations
                    FEANFANG : ANYPTR ;      // anchor to err list
                    FEAKT : ANYPTR ;         // actual err elem
                    FTTAB : ANYPTR ;         // error text table
                    FTTABA : ANYPTR ;        // same for applic.
                    OPTLINE : SOURCELINE ;   // options line
                    POPT : OPTIONS_PTR ;     // ptr to opt struct

     /******************************************/
     /* felder fuer sofortige Protokollausgabe */
     /******************************************/

                    PROTOUT : BOOLEAN ;        // switch for prot out
                    TERMOUT : BOOLEAN ;        // switch for term out
                    FEAKT_ALT : ANYPTR ;       // old feakt
                    LINEINFO : CHAR32 ;        // line information
                    LINEINFO_SIZE : INTEGER ;  // size of lineinfo

     /******************************************/
     /* felder fuer ueberschrift               */
     /******************************************/

                    LINECOUNT : INTEGER ;      // linecount f. heading
                    HEADLINE : SOURCELINE ;    // header line
                    HEADLINE_SIZE : INTEGER ;  // size of header line
                    PAGENR : INTEGER ;         // page number
                  end ;

     /***********************************/
     /* Optionen fuer Compiler          */
     /***********************************/
     /* muss mit Def. beim Scanner      */
     /* uebereinstimmen                 */
     /***********************************/

     COMP_OPTIONS = record
                      LMARGIN : INTEGER ;    // left margin
                      RMARGIN : INTEGER ;    // right margin
                      PAGESIZE : INTEGER ;   // pagesize of listing
                      LIST : BOOLEAN ;       // write listing
                      PRCODE : BOOLEAN ;     // print code
                      GET_STAT : BOOLEAN ;   // get statistics
                      SAVEREGS : BOOLEAN ;   // saveregs
                      SAVEFPRS : BOOLEAN ;   // save fp regs
                      DEBUG : BOOLEAN ;      // debug switch
                      MWARN : BOOLEAN ;      //
                      DEBUG_LEV : 0 .. 9 ;   // debug level
                      NOPACKING : BOOLEAN ;  // no packing
                      NESTCOMM : BOOLEAN ;   // nested comments
                      WARNING : BOOLEAN ;    // show warnings
                      ASSEMBLE : BOOLEAN ;   // show assembly
                      ASMVERB : BOOLEAN ;    // show verbose ass.
                      CTROPTION : BOOLEAN ;  // show counters
                      SHOW_LISTDEF : BOOLEAN ; // show listdef
                    end ;


var I : INTEGER ;
    LIST , PRCODE , PRTABLES : BOOLEAN ; (*output options for *)
                                         (*                   *)
                                         (* -- source program *)
                                         (* listing           *)
                                         (* -- printing symbo *)
                                         (* lic code          *)
                                         (* -- displaying ide *)
                                         (* nt and struct tab *)
                                         (* les               *)
                                         (* -- procedure opti *)
                                         (* ons               *)
                                         (*...................*)



procedure SEARCHID ( FIDCLS : SETOFIDS ; var FCP : CTP ) ;

   label 1 ;

   var LCP : CTP ;

   begin (* SEARCHID *)
     SEARCHIDNE ( FIDCLS , LCP ) ;

     (**********************************************)
     (* perform no                                 *)
     (*                               error search *)
     (**********************************************)

     if LCP <> NIL then
       goto 1 ;               (* found                       *)
                              (*search not successful        *)
                              (*     --> procedure simpletype*)
     ERROR ( 104 ) ;

     (**************************************************)
     (*to avoid returning nil, reference an entry      *)
     (*       for an undeclared id of appropriate class*)
     (*       --> procedure enterundecl                *)
     (**************************************************)

     if TYPES  (* komm in line *) in FIDCLS then
       LCP := UTYPPTR
     else
       if VARS in FIDCLS then
         LCP := UVARPTR
       else
         if FIELD in FIDCLS then
           LCP := UFLDPTR
         else
           if KONST in FIDCLS then
             LCP := UCSTPTR
           else
             if PROC in FIDCLS

     (*******************************************)
     (* long comment which does not fit in line *)
     (*******************************************)

             then
               LCP := UPRCPTR
             else
               LCP := UFCTPTR ;
     1 :
     FCP := LCP
   end (* SEARCHID *) ;



procedure FOLLOWSTP ( FP : STP ) ;

   begin (* FOLLOWSTP *)
     if FP <> NIL then
       with FP -> do
         if MARKED then
           begin
             MARKED := FALSE ;
             WRITE ( OUTPUT , ' ' : 4 , STPTOINT  (*ord*) ( FP ) :
                     INTSIZE  (*6*) , SIZE : 10 ) ;
             case FORM of
               SCALAR :
                 begin
                   WRITE ( OUTPUT , 'scalar' : 10 ) ;
                   if SCALKIND = STANDARD then
                     WRITE ( OUTPUT , 'standard' : 10 )
                   else
                     WRITE ( OUTPUT , 'declared' : 10 , ' ' : 4 ,
                             CTPTOINT  (*ord*) ( FCONST ) : INTSIZE

     (***)
     (*6*)
     (***)

                             ) ;
                   WRITELN ( OUTPUT )
                 end (* tag/ca *) ;
               SUBRANGE :
                 begin
                   WRITE ( OUTPUT , 'subrange' : 10 , ' ' : 4 ,
                           STPTOINT  (*ord*) ( RANGETYPE ) : 6 ) ;
                   if RANGETYPE <> REALPTR then
                     WRITE ( OUTPUT , MIN . IVAL , MAX . IVAL )
                   else
                     if ( MIN . VALP <> NIL ) and ( MAX . VALP <> NIL )
                     then
                       begin
                         WRITE ( ' ' ) ;
                         WRITEV ( OUTPUT , MIN . VALP -> . RVAL , 9 ) ;
                         WRITE ( ' ' ) ;
                         WRITEV ( OUTPUT , MAX . VALP -> . RVAL , 9 )
                       end (* then *) ;
                   WRITELN ( OUTPUT ) ;
                   FOLLOWSTP ( RANGETYPE ) ;
                 end (* tag/ca *) ;
               POINTER :
                 WRITELN ( OUTPUT , 'pointer' : 10 , ' ' : 4 , STPTOINT

     (*****)
     (*ord*)
     (*****)

                           ( ELTYPE ) : INTSIZE  (*6*) ) ;
               POWER : begin
                         WRITELN ( OUTPUT , 'set' : 10 , ' ' : 4 ,
                                   STPTOINT  (*ord*) ( ELSET ) :
                                   INTSIZE  (*6*) ) ;
                         FOLLOWSTP ( ELSET )
                       end (* tag/ca *) ;
               ARRAYS :
                 begin
                   WRITELN ( OUTPUT , 'array' : 10 , ' ' : 4 , STPTOINT

     (*****)
     (*ord*)
     (*****)

                             ( AELTYPE ) : INTSIZE  (*6*) , ' ' : 4 ,
                             STPTOINT  (*ord*) ( INXTYPE ) : 6 ) ;
                   FOLLOWSTP ( AELTYPE ) ;
                   FOLLOWSTP ( INXTYPE )
                 end (* tag/ca *) ;
               RECORDS :
                 begin
                   WRITELN ( OUTPUT , 'record' : 10 , ' ' : 4 ,
                             CTPTOINT  (*ord*) ( FSTFLD ) : INTSIZE

     (***)
     (*6*)
     (***)

                             , ' ' : 4 , STPTOINT  (*ord*) ( RECVAR ) :
                             INTSIZE  (*6*) ) ;
                   FOLLOWCTP ( FSTFLD ) ;
                   FOLLOWSTP ( RECVAR )
                 end (* tag/ca *) ;
               FILES : begin
                         WRITE ( OUTPUT , 'file' : 10 , ' ' : 4 ,
                                 STPTOINT  (*ord*) ( FILTYPE ) :
                                 INTSIZE  (*6*) ) ;
                         FOLLOWSTP ( FILTYPE )
                       end (* tag/ca *) ;
               TAGFLD :
                 begin
                   WRITELN ( OUTPUT , 'tagfld' : 10 , ' ' : 4 ,
                             CTPTOINT  (*ord*) ( TAGFIELDP ) : INTSIZE

     (***)
     (*6*)
     (***)

                             , ' ' : 4 , STPTOINT  (*ord*) ( FSTVAR ) :
                             INTSIZE  (*6*) ) ;
                   FOLLOWSTP ( FSTVAR )
                 end (* tag/ca *) ;
               VARIANT :
                 begin
                   WRITELN ( OUTPUT , 'variant' : 10 , ' ' : 4 ,
                             STPTOINT  (*ord*) ( NXTVAR ) : INTSIZE

     (***)
     (*6*)
     (***)

                             , ' ' : 4 , STPTOINT  (*ord*) ( SUBVAR ) :
                             INTSIZE  (*6*) , VARVAL . IVAL ) ;
                   FOLLOWSTP ( NXTVAR ) ;
                   FOLLOWSTP ( SUBVAR )
                 end (* tag/ca *)
             end (* case *)
           end (* then *)
   end (* FOLLOWSTP *) ;



begin (* HAUPTPROGRAMM *)

  //******************************************************************
  // schauen, ob dieser Kommentar stehen bleibt                       
  //******************************************************************

  I := 11 ;

  //******************************************************************
  // test neuer kommentar                                             
  //******************************************************************


  //******************************************************************
  // test zwei kommentare                                             
  //******************************************************************

  I := 12 ;

  (******************************************************************)
  (*output options for                                              *)
  (*      -- source program listing                                 *)
  (*      -- printing symbolic code                                 *)
  (*      -- displaying ident and struct tables                     *)
  (*      --> procedure option                                      *)
  (******************************************************************)

end (* HAUPTPROGRAMM *) .
