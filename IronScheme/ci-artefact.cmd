echo on
setlocal  EnableDelayedExpansion

if "%APPVEYOR_BUILD_VERSION%"=="" set APPVEYOR_BUILD_VERSION=0.0.0
if "%SHA%"=="" set SHA=000000
if "%APPVEYOR_BUILD_FOLDER%"=="" set APPVEYOR_BUILD_FOLDER=%CD%

cd bin
ren IronScheme-latest.zip IronScheme-%APPVEYOR_BUILD_VERSION%-%SHA%.zip
rem ren IronScheme-latest-DEBUG.zip IronScheme-%APPVEYOR_BUILD_VERSION%-%SHA%-DEBUG.zip

set BINDIR=%CD%

pushd ..\IronScheme.Console\bin\Release\net20\install-stage

copy /y ..\..\..\..\IronScheme.*.nuspec .
nuget pack IronScheme.Core.nuspec -properties version=%APPVEYOR_BUILD_VERSION%;sha=%SHA%
nuget pack IronScheme.Tool.nuspec -properties version=%APPVEYOR_BUILD_VERSION%;sha=%SHA%
copy /y IronScheme*.*nupkg %BINDIR%

popd

if "%APPVEYOR_BUILD_FOLDER%\bin"=="%CD%" exit /b %ERRORLEVEL%

copy IronScheme-%APPVEYOR_BUILD_VERSION%-%SHA%*.zip %APPVEYOR_BUILD_FOLDER%
copy IronScheme.*.%APPVEYOR_BUILD_VERSION%*.*nupkg %APPVEYOR_BUILD_FOLDER%
cd %APPVEYOR_BUILD_FOLDER%
@echo off
