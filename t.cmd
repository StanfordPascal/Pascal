rem call pasf comperrf
rem call pp comperrf
rem set dd_pcode=testch01.pcode
rem call prun comperrf
rem goto :ende

    call pasf pascal3
    call pp pascal3
    call t2 testch01
    call t2 testset7
    call t2 testhex

:ende
@echo off

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
rem call pasf xcompn
rem call pasf paslibx
rem call pp xcompn
rem call pp paslibx
rem call xcompn xcompn.pas xcompn.v1
rem call xcompn AUSGABE.TFIN2002 AUSGABE.TFIN2202
rem call xcompn AUSGABE.TFUP2002 AUSGABE.TFUP2202
