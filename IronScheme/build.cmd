@echo off
setlocal

if "%1" == "/?" goto help
if "%1" == "-h" goto help
if "%1" == "--help" goto help

set FX=%1
if "%FX%" == "" set FX=net20
set CFG=%2
if "%CFG%" == "" set CFG=Release



echo.
echo Building IronScheme with .NET %FX% - %CFG% 
echo.

msbuild IronSchemeCore.sln /v:m /p:TargetFramework=%FX% /p:Configuration=%CFG%
exit /B %ERRORLEVEL%

:help
echo Usage: build [FrameworkTarget [Configuration]]
exit /B 0

:err
echo Invalid options, see build /?
exit /B 1
