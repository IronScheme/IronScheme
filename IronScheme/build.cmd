@echo off
setlocal

if "%1" == "/?" goto help
if "%1" == "-h" goto help
if "%1" == "--help" goto help

rem `dotnet build` or `dotnet msbuild -restore` also works instead of `msbuild -restore`
msbuild -restore /v:m /p:Configuration=Release %*
exit /B %ERRORLEVEL%

:help
echo Usage: build [msbuild args]
exit /B 0

:err
echo Invalid options, see build /?
exit /B 1
