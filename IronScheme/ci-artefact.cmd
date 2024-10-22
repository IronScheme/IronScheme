echo on
setlocal  EnableDelayedExpansion

if "%APPVEYOR_BUILD_VERSION%"=="" set APPVEYOR_BUILD_VERSION=0.0.0
if "%SHA%"=="" set SHA=000000
if "%APPVEYOR_BUILD_FOLDER%"=="" set APPVEYOR_BUILD_FOLDER=%CD%

ren bin\IronScheme-latest.zip IronScheme-%APPVEYOR_BUILD_VERSION%-%SHA%.zip

call nuget.cmd

echo on

if "%APPVEYOR_BUILD_FOLDER%"=="%CD%" exit /b %ERRORLEVEL%

copy bin\IronScheme-%APPVEYOR_BUILD_VERSION%-%SHA%*.zip %APPVEYOR_BUILD_FOLDER%
copy bin\IronScheme.*.%APPVEYOR_BUILD_VERSION%*.*nupkg %APPVEYOR_BUILD_FOLDER%
cd %APPVEYOR_BUILD_FOLDER%
@echo off
