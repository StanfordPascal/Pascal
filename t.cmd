rem call pp pascal2
rem call pas2 teststr0
rem pause
rem call x teststr0.asmout
rem call pp testlit
rem call pp testlitf
rem call pas2 testlit
rem pause
rem call x testlit.asmout
rem call pas2 testlitf
rem pause
rem call x testlitf.asmout
rem call pas2 testmcmp
rem pause
rem call x testmcmp.asmout
rem call pas2 paslibx
rem pause
rem call x paslibx.asmout
rem ********************************************************
rem call pasf avltree
rem call pasf testavl
rem call pp avltree
rem call pp testavl
rem call prun testavl avltree >xxx
rem call xa xxx
rem ********************************************************
    call pasf xcompn
    call pasf paslibx
    call pp xcompn
    call pp paslibx
    call xcompn xcompn.pas xcompn.v1
rem call xcompn AUSGABE.TFIN2002 AUSGABE.TFIN2202
    call xcompn AUSGABE.TFUP2002 AUSGABE.TFUP2202
