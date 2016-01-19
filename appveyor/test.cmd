set PATH=%PATH%;"C:\Program Files (x86)\Microsoft SDKs\Windows\v10.0A\bin\NETFX 4.6.1 Tools\";

cd IronScheme\IronScheme.Console\bin\Release
mklink isc.exe IronScheme.Console32.exe

REM appveyor AddTest "Release bootstrap" -Outcome Running
isc ironscheme-buildscript.sps
@IF %ERRORLEVEL% NEQ 0 exit /b 1
isc ironscheme-buildscript.sps
peverify /nologo ironscheme.boot.dll
@IF %ERRORLEVEL% NEQ 0 exit /b 1
REM appveyor UpdateTest "Release bootstrap" -Outcome Passed

REM appveyor AddTest "Release libraries" -Outcome Running
isc compile-system-libraries.sps
ren lib lib.hide
isc --show-loaded-libraries compile-system-libraries.sps > libraries.lst
ren lib.hide lib
REM appveyor UpdateTest "Release libraries" -Outcome Passed

REM appveyor AddTest "Debug bootstrap" -Outcome Running
isc -debug ironscheme-buildscript.sps
@IF %ERRORLEVEL% NEQ 0 exit /b 1
isc -debug ironscheme-buildscript.sps
peverify /nologo /ignore=0x80131820 ironscheme.boot.dll
@IF %ERRORLEVEL% NEQ 0 exit /b 1
REM appveyor UpdateTest "Debug bootstrap" -Outcome Passed

REM appveyor AddTest "Debug libraries" -Outcome Running
FOR /F %%f IN ('type libraries.lst') DO @del %%f
isc -debug compile-system-libraries.sps
ren lib lib.hide
isc -debug compile-system-libraries.sps
FOR /F %%F IN ('type libraries.lst') DO @del %%~pnF.pdb
ren lib.hide lib
REM appveyor UpdateTest "Debug libraries" -Outcome Passed

REM appveyor AddTest "Release bootstrap for test" -Outcome Running
isc ironscheme-buildscript.sps
FOR /F %%f IN ('type libraries.lst') DO @del %%f
REM appveyor UpdateTest "Release bootstrap for test" -Outcome Passed

REM appveyor AddTest "Release libraries for test" -Outcome Running
isc compile-system-libraries.sps
REM appveyor UpdateTest "Release libraries for test" -Outcome Passed

REM appveyor AddTest "R6RS" -Outcome Running
isc tests\r6rs\run.sps
isc tests\trigtest.sps
isc tests\clisp-number-tests.sps
isc tests\fp-test.sps
REM appveyor UpdateTest "R6RS" -Outcome Passed

REM appveyor AddTest "SRFI" -Outcome Running
isc lib\srfi\tests\and-let_.sps
isc lib\srfi\tests\compare-procedures.sps
isc lib\srfi\tests\cut.sps
isc lib\srfi\tests\eager-comprehensions.sps
isc lib\srfi\tests\intermediate-format-strings.sps
isc lib\srfi\tests\lightweight-testing.sps
isc lib\srfi\tests\lists.sps
isc lib\srfi\tests\multi-dimensional-arrays--arlib.sps
isc lib\srfi\tests\multi-dimensional-arrays.sps
isc lib\srfi\tests\os-environment-variables.sps
isc lib\srfi\tests\procedure-arity.sps
isc lib\srfi\tests\print-ascii.sps
rem isc lib\srfi\tests\random-conftest.sps
isc lib\srfi\tests\random.sps
isc lib\srfi\tests\random-access-lists.sps
isc lib\srfi\tests\rec-factorial.sps
isc lib\srfi\tests\records.sps
isc lib\srfi\tests\testing.sps
isc lib\srfi\tests\time.sps
REM appveyor UpdateTest "SRFI" -Outcome Passed

REM appveyor AddTest "PFDS" -Outcome Running
isc lib\pfds\tests.scm
REM appveyor UpdateTest "PFDS" -Outcome Passed

REM appveyor AddTest "MiniKanren" -Outcome Running
echo (include "lib/minikanren/mktests.scm") | isc
REM appveyor UpdateTest "MiniKanren" -Outcome Passed

FOR /F %%f IN ('type libraries.lst') DO @del %%f