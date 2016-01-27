@echo off
setlocal

rem these have to be in order
set TESTS=Release,Debug,Setup,Conformance,SRFI,Other,Teardown

rem set this to anything if you want dont want verbose output
set QUIET=1

rem set nunit bin directory
set NUNIT_PATH=%USERPROFILE%\Downloads\NUnit-2.6.4\NUnit-2.6.4\bin\

rem setup path
set PATH=%PATH%;%NUNIT_PATH%;

rem checks
where peverify >nul 2>&1
IF %ERRORLEVEL% NEQ 0 goto no_peverify
where nunit-console-x86 >nul 2>&1
IF %ERRORLEVEL% NEQ 0 goto no_nunit


set NUNIT=call :runtest

cd IronScheme.Console\bin\Release
md results 2> nul

for %%t in (%TESTS%) do %NUNIT% %%t 

cd ..\..\..
exit /b 0

:runtest
@echo on
nunit-console-x86.exe /nologo /labels /framework:net-2.0 ^
/work:results /result:%1.xml ^
IronScheme.Tests.dll /run:IronScheme.Tests.%1
@echo off 
goto :eof

:no_peverify
echo Error: PEVerify not found in PATH
exit /b 1

:no_nunit
echo Error: NUnit path not set correctly
exit /b 1

