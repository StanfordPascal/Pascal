program TESTX ( INPUT , OUTPUT ) ;


const SIZEDSN = 44 ;


var DSN : CHAR ( SIZEDSN ) ;
    T2 : array [ 12 .. 10 ] of CHAR ;
    T3 : array [ 10 .. 12 ] of CHAR ( SIZEDSN ) ;


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


begin (* HAUPTPROGRAMM *)
  T2 [12] := 'A';
  DSN := DSN_TAB [ 13 ] ;
end (* HAUPTPROGRAMM *) .
