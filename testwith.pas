program TESTWITH ( OUTPUT ) ;

//********************************
//$X+
//********************************



const NIL2 = NIL ;
      MAXADDR = 16777215 ;
      MAXLEVEL = 9 ;
      IDLNGTH = 20 ;
      EXTNAMSZ = 8 ;


type ALPHA = array [ 1 .. IDLNGTH ] of CHAR ;
     EXTNAMTP = array [ 1 .. EXTNAMSZ ] of CHAR ;
     TTP = -> TYPEREC ;
     LEVRANGE = 0 .. MAXLEVEL ;
     ADDRRANGE = 0 .. MAXADDR ;
     LABELRNG = 0 .. 1000 ;
     DECLKIND = ( STANDARD , DECLARED ) ;
     IDCLASS = ( TYPES , KONST , STRUCTKONST , VARS , FIELD , PROC ,
               FUNC ) ;
     IDKIND = ( NORMALVAR , VALUEPARM , VARPARM , CONSTPARM ) ;
     STORAGE_CLASS = ( XAUTO , XSTATIC ) ;
     TYPEREC = record
                 SIZE : ADDRRANGE ;
                 ERRORFLAG : BOOLEAN ;
               end ;
     XCONSTANT = record
                   STRTYPE : CHAR ;
                 end ;
     IDENTIFIER = record
                    NAME : ALPHA ;
                    IDTYPE : TTP ;
                    NEXT_IN_BKT : -> IDENTIFIER ;
                    NEXT : -> IDENTIFIER ;
                    PREV_VAR_IN_BLOCK : -> IDENTIFIER ;
                    PREV_TYPE_IN_BLOCK : -> IDENTIFIER ;
                    PREV_PROC_IN_BLOCK : -> IDENTIFIER ;
                    DECL_LEV : LEVRANGE ;
                    case KLASS : IDCLASS of
                      TYPES :
                        ( REFERENCET : CHAR ) ;
                      KONST :
                        ( VALUES : XCONSTANT ) ;
                      STRUCTKONST :
                        ( SKOWNERPROC : EXTNAMTP ;
                          SKADDR : ADDRRANGE ) ;
                      VARS :
                        ( VKIND : IDKIND ;
                          VLEV : LEVRANGE ;
                          STKLASS : STORAGE_CLASS ;
                          REFERENCE : CHAR ;
                          VOWNERPROC : EXTNAMTP ;
                          VADDR : ADDRRANGE ;
                          SPECIAL : INTEGER ;
                          DUMMYVAR : BOOLEAN ;
                          DUMMYLEV : LEVRANGE ;
                          DUMMYADDR : ADDRRANGE ) ;
                      FIELD :
                        ( FLDADDR : ADDRRANGE ;
                          OWNER : TTP ) ;
                      PROC , FUNC :
                        ( EXTRN : BOOLEAN ;
                          EXTLANG : CHAR ;
                          EXTNAME : EXTNAMTP ;
                          REFERENCEP : CHAR ;
                          case PFDECKIND : DECLKIND of
                            STANDARD :
                              ( KEY : INTEGER ;
                                LIBNAME : EXTNAMTP ;
                                FUNCCODE : INTEGER ;
                                PARMCNT : INTEGER ;
                                WASIZE : INTEGER ) ;
                            DECLARED :
                              ( FWDECL : BOOLEAN ;
                                PFLEV : INTEGER ;
                                PFNAME : LABELRNG ;
                                PRMPTR , NXTFWRD : -> IDENTIFIER ;
                                PFKIND : IDKIND ;
                                DECLMISSING : BOOLEAN ;
                                CSTNAME : EXTNAMTP ) )
                  end ;


const NIL3 = NIL ;


type IDENT_BASE = record
                    NAME : ALPHA ;
                    IDTYPE : TTP ;
                    NEXT_IN_BKT : -> IDENTIFIER ;
                    NEXT : -> IDENTIFIER ;
                    PREV_VAR_IN_BLOCK : -> IDENTIFIER ;
                    PREV_TYPE_IN_BLOCK : -> IDENTIFIER ;
                    PREV_PROC_IN_BLOCK : -> IDENTIFIER ;
                    DECL_LEV : LEVRANGE ;
                  end ;
     IDENT_TYPE_EXT = record
                        REFERENCET : CHAR ;
                      end ;
     IDENT_KONST_EXT = record
                         VALUES : XCONSTANT ;
                       end ;
     IDENT_STKONST_EXT = record
                           SKOWNERPROC : EXTNAMTP ;
                           SKADDR : ADDRRANGE ;
                         end ;
     IDENT_VAR_EXT = record
                       VKIND : IDKIND ;
                       VLEV : LEVRANGE ;
                       STKLASS : STORAGE_CLASS ;
                       REFERENCE : CHAR ;
                       VOWNERPROC : EXTNAMTP ;
                       VADDR : ADDRRANGE ;
                       SPECIAL : INTEGER ;
                       DUMMYVAR : BOOLEAN ;
                       DUMMYLEV : LEVRANGE ;
                       DUMMYADDR : ADDRRANGE ;
                     end ;
     IDENT_FIELD_EXT = record
                         FLDADDR : ADDRRANGE ;
                         OWNER : TTP ;
                       end ;
     IDENT_PROC_EXT = record
                        EXTRN : BOOLEAN ;
                        EXTLANG : CHAR ;
                        EXTNAME : EXTNAMTP ;
                        REFERENCEP : CHAR ;
                        case PFDECKIND : DECLKIND of
                          STANDARD :
                            ( KEY : INTEGER ;
                              LIBNAME : EXTNAMTP ;
                              FUNCCODE : INTEGER ;
                              PARMCNT : INTEGER ;
                              WASIZE : INTEGER ) ;
                          DECLARED :
                            ( FWDECL : BOOLEAN ;
                              PFLEV : INTEGER ;
                              PFNAME : LABELRNG ;
                              PRMPTR , NXTFWRD : -> IDENTIFIER ;
                              PFKIND : IDKIND ;
                              DECLMISSING : BOOLEAN ;
                              CSTNAME : EXTNAMTP )
                      end ;


const NIL35 = NIL ;


type IDENT_NEW = record
                   with BASE : IDENT_BASE ;
                   KLASS : IDCLASS ;
                   with PTYPE : -> IDENT_TYPE_EXT ;
                   with PKONST : -> IDENT_KONST_EXT ;
                   with PSTKONST : -> IDENT_STKONST_EXT ;
                   with PVAR : -> IDENT_VAR_EXT ;

     //************************************************************
     //   with PVAR2 : -> IDENT_VAR_EXT ;
     //************************************************************

                   with PFIELD : -> IDENT_FIELD_EXT ;
                   with PPROC : -> IDENT_PROC_EXT ;
                 end ;


const NIL4 = NIL ;


type S1 = record
            X : CHAR ( 27 ) ;
            case INTEGER of
              1 :
                ( V : INTEGER ) ;
              2 :
                ( W : REAL ) ;
              3 :
                ( Z : CHAR ( 100 ) ) ;
              4 :
                ( case BOOLEAN of
                    TRUE :
                      ( A1 : CHAR ( 5 ) ) ;
                    FALSE :
                      ( A2 : CHAR ( 7 ) ) )
          end ;
     S2 = record
            A : INTEGER ;
            with B : S1 ;
            C : INTEGER ;
          end ;
     S3 = record
            D : INTEGER ;
            with F : S2 ;
            E : INTEGER ;
          end ;
     X1 = record
            case INTEGER of
              1 :
                ( P : VOIDPTR ) ;
              2 :
                ( I : INTEGER )
          end ;


const NIL5 = NIL ;


var ID : IDENT_NEW ;
    S : S2 ;
    T : S3 ;
    U : S1 ;
    B : BOOLEAN ;
    DUMMYNIL : X1 ;


const NIL6 = NIL ;


begin (* HAUPTPROGRAMM *)
  DUMMYNIL . I := - 1 ;
  ID . NAME := 'HUGO' ;
  ID . KLASS := VARS ;
  WRITELN ( 'test1: ' , ID . NAME , ID . KLASS ) ;
  B := FALSE ;
  WRITELN ( 'pvar: ' , ID . PVAR ) ;
  WRITELN ( 'pvar: ' , ID . PVAR ) ;
  ID . PVAR := DUMMYNIL . P ;
  ID . PVAR := NIL ;

  //******************************************************************
  // ID . PVAR := ALLOC ( SIZEOF ( IDENT_VAR_EXT ) ) ;
  //******************************************************************

  WRITELN ( 'pvar: ' , ID . PVAR ) ;
  NEW ( ID . PVAR ) ;
  WRITELN ( 'pvar: ' , ID . PVAR ) ;
  ID . VLEV := 1 ;
  ID . REFERENCE := 'X' ;
  WRITELN ( 'test2: ' , ID . VLEV , ID . REFERENCE : 5 ) ;
  WRITELN ( 'test3: ' , ID . NAME , ID . KLASS ) ;
  U . A1 := 'Bernd' ;
  S . A2 := 'Bernd' ;
  T . A2 := 'Bernd' ;
  S . A := 1 ;
  S . X := 'Bernd' ;
  S . C := 3 ;
  S . V := 5 ;
  S . Z := '*****' ;
  S . B . V := 5 ;
  T . A := 1 ;
  T . X := 'Bernd' ;
  T . V := 7 ;
  T . Z := '*****' ;
  T . B . V := 7 ;
  T . C := 3 ;
  T . D := 4 ;
  T . E := 5 ;
  T . F . A := 7 ;
  T . F . X := 'Bernd2' ;
  T . B . X := 'Bernd3' ;
  with T do
    WRITELN ( A , C , D , E , ' ' , X ) ;
  with S do
    WRITELN ( A , C , ' ' , X ) ;
end (* HAUPTPROGRAMM *) .
