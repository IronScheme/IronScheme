@echo off
setlocal

if "%1" == "/?" goto help
if "%1" == "-h" goto help
if "%1" == "--help" goto help

dotnet build --disable-build-servers -tl:off -c Release ../IronScheme.BuildTools/IronScheme.Build/IronScheme.Build.csproj
dotnet build --disable-build-servers -tl:off -c Release -f net4.0 ../IronScheme.BuildTools/Setup/Setup.csproj

rem `dotnet build` or `dotnet msbuild -restore` also works instead of `msbuild -restore`
dotnet build --disable-build-servers -tl:off -c Release %*
exit /B %ERRORLEVEL%

:help
echo Usage: build [msbuild args]
exit /B 0

:err
echo Invalid options, see build /?
exit /B 1
