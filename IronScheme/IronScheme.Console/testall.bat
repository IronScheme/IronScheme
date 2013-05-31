isc ironscheme-buildscript.sps
@IF %ERRORLEVEL% NEQ 0 exit /b 1
isc ironscheme-buildscript.sps
peverify /nologo ironscheme.boot.dll
@IF %ERRORLEVEL% NEQ 0 exit /b 1
isc compile-system-libraries.sps
isc compile-system-libraries.sps
isc -debug ironscheme-buildscript.sps
@IF %ERRORLEVEL% NEQ 0 exit /b 1
isc -debug ironscheme-buildscript.sps
peverify /nologo /ignore=0x80131820 ironscheme.boot.dll
@IF %ERRORLEVEL% NEQ 0 exit /b 1
isc -debug compile-system-libraries.sps
isc -debug compile-system-libraries.sps
isc ironscheme-buildscript.sps
isc compile-system-libraries.sps
call run-tests.bat
call srfitest.bat
isc lib\pfds\tests.scm > pfds.log
echo (include "lib/minikanren/mktests.scm") | isc > mktest.log

