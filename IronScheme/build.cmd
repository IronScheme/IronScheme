@echo off
setlocal

if "%1" == "/?" goto help
if "%1" == "-h" goto help
if "%1" == "--help" goto help

SET COMMON=-m -c Release -clp:NoSummary

dotnet build %COMMON% ../IronScheme.BuildTools/IronScheme.Build/IronScheme.Build.csproj -v:q
dotnet build %COMMON% --p:TargetFramework=net4.0 ../IronScheme.BuildTools/Setup/Setup.csproj -v:q 

dotnet build %COMMON% -bl:build.binlog %*
exit /B %ERRORLEVEL%

:help
echo Usage: build [msbuild args]
exit /B 0

:err
echo Invalid options, see build /?
exit /B 1
