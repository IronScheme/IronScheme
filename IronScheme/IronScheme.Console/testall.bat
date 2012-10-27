call isc ironscheme-buildscript.sps
@IF %ERRORLEVEL% NEQ 0 exit /b 1
call isc ironscheme-buildscript.sps
peverify /nologo ironscheme.boot.dll
@IF %ERRORLEVEL% NEQ 0 exit /b 1
call isc compile-system-libraries.sps
call isc compile-system-libraries.sps
call isc -debug ironscheme-buildscript.sps
@IF %ERRORLEVEL% NEQ 0 exit /b 1
call isc -debug ironscheme-buildscript.sps
peverify /nologo /ignore=0x80131820 ironscheme.boot.dll
@IF %ERRORLEVEL% NEQ 0 exit /b 1
call isc -debug compile-system-libraries.sps
call isc -debug compile-system-libraries.sps
call isc ironscheme-buildscript.sps
call isc compile-system-libraries.sps
call run-tests.bat
call srfitest.bat
call isc lib\pfds\tests.scm > pfds.log
