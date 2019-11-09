call pasf pasform
call pp pasform
del testkomm.pas
copy testkomm.v1 *.pas
call pasf testkomm
copy pascal1.bak pascal1.pas
call pasf pascal1
xcomp pascal1.pas pascal1.bak >xxx
call xa xxx
