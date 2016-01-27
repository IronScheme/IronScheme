@echo off
setlocal

rem these have to be in order
set TESTS=Release,Debug,Setup,Conformance,SRFI,Other,Teardown

rem set this to anything if you want dont want verbose output
set QUIET=1

rem set the path to nunit
set PATH=%PATH%;%USERPROFILE%\Downloads\NUnit-2.6.4\NUnit-2.6.4\bin\;

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