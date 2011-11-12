call isc ironscheme-buildscript.ss
call isc ironscheme-buildscript.ss
peverify /nologo ironscheme.boot.dll
call isc compile-system-libraries.sps
call isc compile-system-libraries.sps
call isc -debug ironscheme-buildscript.ss
call isc -debug ironscheme-buildscript.ss
peverify /nologo /ignore=0x80131820 ironscheme.boot.dll
call isc -debug compile-system-libraries.sps
call isc -debug compile-system-libraries.sps
call isc ironscheme-buildscript.ss
call isc compile-system-libraries.sps
call run-tests.bat
call srfitest.bat
