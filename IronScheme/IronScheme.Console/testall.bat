@set PATH="c:\Program Files (x86)\Microsoft SDKs\Windows\v8.0A\bin\NETFX 4.0 Tools\";%PATH%
 
call isc ironscheme-buildscript.ss
@IF %ERRORLEVEL% NEQ 0 exit /b 1
call isc ironscheme-buildscript.ss
peverify /nologo ironscheme.boot.dll
@IF %ERRORLEVEL% NEQ 0 exit /b 1
call isc compile-system-libraries.sps
call isc compile-system-libraries.sps
call isc -debug ironscheme-buildscript.ss
@IF %ERRORLEVEL% NEQ 0 exit /b 1
call isc -debug ironscheme-buildscript.ss
peverify /nologo /ignore=0x80131820 ironscheme.boot.dll
@IF %ERRORLEVEL% NEQ 0 exit /b 1
call isc -debug compile-system-libraries.sps
call isc -debug compile-system-libraries.sps
call isc ironscheme-buildscript.ss
call isc compile-system-libraries.sps
call run-tests.bat
call srfitest.bat
call isc lib\pfds\tests.scm > pfds.log
