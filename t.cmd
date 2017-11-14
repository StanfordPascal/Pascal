call pasf pasform
call pp pasform
copy pasform.pas x.pas
call pasf x
xcomp x.pas pasform.pas
pause
del t.pas
copy t2.pas t.pas
call pasf t
x t.pas
