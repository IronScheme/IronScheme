@echo off
setlocal

if "%1" == "/?" goto help
if "%1" == "-h" goto help
if "%1" == "--help" goto help

if "%1" == "--no-restore" (
  dotnet  build -c Release -m %*
  goto :eof
)

SET COMMON=-m -c Release -clp:NoSummary --disable-build-servers

SET BUILD=dotnet build %COMMON% --p:TargetFramework=netstandard2.0 ../IronScheme.BuildTools/IronScheme.Build/IronScheme.Build.csproj 
SET TOOLS=dotnet build %COMMON% --p:TargetFramework=net4.0 ../IronScheme.BuildTools/Setup/Setup.csproj 
SET PROJ=dotnet build %COMMON% -bl:build.binlog %*

endLocal & goto #_undefined_# 2>NUL || %BUILD% && %TOOLS% && %PROJ%

:help
echo Usage: build [msbuild args]
exit /B 0

:err
echo Invalid options, see build /?
exit /B 1
