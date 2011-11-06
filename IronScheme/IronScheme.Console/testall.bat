call isc ironscheme-buildscript.ss
call isc ironscheme-buildscript.ss
peverify ironscheme.boot.dll
call isc compile-system-libraries.sps
call isc compile-system-libraries.sps
call isc -debug ironscheme-buildscript.ss
call isc -debug ironscheme-buildscript.ss
call isc -debug compile-system-libraries.sps
call isc -debug compile-system-libraries.sps
call isc ironscheme-buildscript.ss
call isc compile-system-libraries.sps
call run-tests.bat
call srfitest.bat
