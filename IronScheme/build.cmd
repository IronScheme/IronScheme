@echo off
setlocal

if "%1" == "/?" goto help
if "%1" == "-h" goto help
if "%1" == "--help" goto help

set CFG=%1
if "%CFG%" == "" set CFG=Release

dotnet build IronSchemeCore.sln /v:m /p:Configuration=%CFG%
exit /B %ERRORLEVEL%

:help
echo Usage: build [Configuration]
exit /B 0

:err
echo Invalid options, see build /?
exit /B 1
