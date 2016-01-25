@echo off
setlocal

if "%1" == "/?" goto help
if "%1" == "-h" goto help
if "%1" == "--help" goto help

set FX=%1
if "%FX%" == "" set FX=v2.0
set CFG=%2
if "%CFG%" == "" set CFG=Release



echo.
echo Building IronScheme with .NET %FX% - %CFG% 
echo.

msbuild IronSchemeCore.sln /v:m /p:TargetFrameworkVersion=%FX% /p:Configuration=%CFG%
exit /B %ERRORLEVEL%

:help
echo Usage: build [FrameworkVersion [Configuration]]
echo - FrameworkVersion: v2.0 (default), v4.0, v.4.5
echo - Configuration: Release (default), Debug
exit /B 0

:err
echo Invalid options, see build /?
exit /B 1
