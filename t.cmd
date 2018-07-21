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
    call pasf avltree
    call pasf testavl
    call pp avltree
    call pp testavl
    call prun testavl avltree >xxx
    call xa xxx
