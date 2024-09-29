@echo off
setlocal

rem set nunit bin directory
set NUNIT_PATH=d:\Downloads\NUnit.Console-3.18.2\bin\net462\

rem these have to be in order
rem set TESTS=IronScheme.Tests.Bootstrap,IronScheme.Tests.Conformance,IronScheme.Tests.SRFI,IronScheme.Tests.Other,IronScheme.Tests.Teardown
rem set TESTS=Bootstrap.Debug,Compile.Debug,Verify.Debug,Bootstrap.Release,Compile.Release,Verify.Release,Conformance,SRFI,Other,Teardown


set QUIET=1
set ARGS=%*
set FX=net20
set TESTCORE=0

rem the args you want to handle
set MYARGS=verbose V core net9

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

:net9
set TESTCORE=1
set FX=net9.0
goto :eof

:script
rem setup path
set PATH=%PATH%;%NUNIT_PATH%;

rem checks
where peverify >nul 2>&1
IF %ERRORLEVEL% NEQ 0 goto no_peverify
where nunit3-console >nul 2>&1
IF %ERRORLEVEL% NEQ 0 goto no_nunit

IF %TESTCORE% == 1 IF %FX% neq net9.0 set TESTS=--test=IronScheme.Tests.Conformance,IronScheme.Tests.SRFI,IronScheme.Tests.Other

rem set TESTS=--test=IronScheme.Tests.Other

rem set NUNIT=call :runtest

cd IronScheme.Console\bin\Release\%FX%

rem IF %TESTCORE% == 1 copy /y ..\net20\ironscheme.boot.dll .

rem copy /y ..\..\..\..\IronScheme.Tests\bin\Release\*.dll .


nunit3-console --noh --labels=BeforeAndAfter --workers=8 --noresult --stoponerror %TESTS% ..\..\..\..\IronScheme.Tests\bin\Release\IronScheme.Tests.dll

cd ..\..\..
exit /b 0

:no_peverify
echo Error: PEVerify not found in PATH
exit /b 1

:no_nunit
echo Error: NUnit path not set correctly
exit /b 1

