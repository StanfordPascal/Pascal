
rem **********************************************
rem *** alte Verzeichnisse loeschen
rem **********************************************

rd PASCALN_COMPILER_CNTL        /s /q
rd PASCALN_COMPILER_MESSAGES    /s /q
rd PASCALN_COMPILER_PAS         /s /q
rd PASCALN_COMPILER_PROCLIB     /s /q
rd PASCALN_COMPILER_TEXT        /s /q
rd PASCALN_RUNTIME_ASM          /s /q
rd PASCALN_RUNTIME_TEXT         /s /q
rd PASCALN_TESTPGM_CNTL         /s /q
rd PASCALN_TESTPGM_PAS          /s /q
rd PASCALN_TESTPGM_ASM          /s /q

rem **********************************************
rem *** neu anlegen anhand PASCALN.TXT
rem **********************************************

set dd_input=pascaln.txt
call prun splitpas
set dd_input=

