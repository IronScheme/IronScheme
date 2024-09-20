@echo off
setlocal

if "%1" == "/?" goto help
if "%1" == "-h" goto help
if "%1" == "--help" goto help

set CFG=%1
if "%CFG%" == "" set CFG=Release

rem `dotnet build` or `dotnet msbuild -restore` also works instead of `msbuild -restore`
msbuild -restore IronSchemeCore.sln /v:m /p:Configuration=%CFG% /p:NoWarn="NETSDK1138;NU1702;NU1902;NU1903;NU1701;CS3021;SYSLIB0050"
exit /B %ERRORLEVEL%

:help
echo Usage: build [Configuration]
exit /B 0

:err
echo Invalid options, see build /?
exit /B 1
