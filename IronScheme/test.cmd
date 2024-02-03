@echo off
setlocal

set QUIET=1
set ARGS=%*
set FX=net472
set TESTCORE=0

rem the args you want to handle
set MYARGS=verbose V core

rem the prefix for arg
set PREFIX=/

rem implementation start
for %%m in (%MYARGS%) do call :parse %%m
goto script

:parse
for %%a in (%ARGS%) do for /f "tokens=1,2 delims=:" %%b in ("%%a") do if %%b == %PREFIX%%1 call :%1 %%c
goto :eof
rem implementation end

:verbose
:V
set QUIET=
goto :eof

:core
set TESTCORE=1
set FX=netcoreapp2.1
goto :eof

:script
rem checks
where peverify >nul 2>&1
IF %ERRORLEVEL% NEQ 0 goto no_peverify

cd IronScheme.Console\bin\Release\%FX%

IF %TESTCORE% == 1 copy /y ..\net472\ironscheme.boot.dll .

@echo on
dotnet test IronScheme.Tests.dll
@echo off

cd ..\..\..
exit /b 0

:runtest
goto :eof

:no_peverify
echo Error: PEVerify not found in PATH
exit /b 1

