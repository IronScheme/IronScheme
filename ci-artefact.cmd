echo on

if "%APPVEYOR_BUILD_VERSION%"=="" set APPVEYOR_BUILD_VERSION="0.0.0"
if "%SHA%"=="" set SHA="000000"

cd IronScheme
call package.cmd

cd bin
ren IronScheme-latest.zip IronScheme-%APPVEYOR_BUILD_VERSION%-%SHA%.zip
ren IronScheme-latest-DEBUG.zip IronScheme-%APPVEYOR_BUILD_VERSION%-%SHA%-DEBUG.zip

set BINDIR=%CD%

pushd ..\IronScheme.Console\bin\Release\net20\install-stage

copy /y ..\..\..\..\IronScheme.Core.nuspec .
nuget pack IronScheme.Core.nuspec -properties version=%APPVEYOR_BUILD_VERSION%;sha=%SHA%
copy /y IronScheme*.*nupkg %BINDIR%

popd

copy IronScheme-%APPVEYOR_BUILD_VERSION%-%SHA%*.* %APPVEYOR_BUILD_FOLDER%
copy IronScheme.Core.%APPVEYOR_BUILD_VERSION%.*nupkg %APPVEYOR_BUILD_FOLDER%
cd %APPVEYOR_BUILD_FOLDER%
@echo off
