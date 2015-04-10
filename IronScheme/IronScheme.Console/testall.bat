isc ironscheme-buildscript.sps
@IF %ERRORLEVEL% NEQ 0 exit /b 1
isc ironscheme-buildscript.sps
peverify /nologo ironscheme.boot.dll
@IF %ERRORLEVEL% NEQ 0 exit /b 1
isc compile-system-libraries.sps
ren lib lib.hide
isc --show-loaded-libraries compile-system-libraries.sps > libraries.lst
ren lib.hide lib
isc -debug ironscheme-buildscript.sps
@IF %ERRORLEVEL% NEQ 0 exit /b 1
isc -debug ironscheme-buildscript.sps
peverify /nologo /ignore=0x80131820 ironscheme.boot.dll
@IF %ERRORLEVEL% NEQ 0 exit /b 1
isc -debug compile-system-libraries.sps
ren lib lib.hide
isc -debug compile-system-libraries.sps
FOR /F %%F IN ('type libraries.lst') DO @del %%~pnF.pdb
ren lib.hide lib
isc ironscheme-buildscript.sps
isc compile-system-libraries.sps
call run-tests.bat
call srfitest.bat
isc lib\pfds\tests.scm > pfds.log
echo (include "lib/minikanren/mktests.scm") | isc > mktest.log

FOR /F %%f IN ('type libraries.lst') DO @del %%f

