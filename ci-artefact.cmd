echo on
cd IronScheme
call package.cmd
cd bin
ren IronScheme-latest.zip IronScheme-%APPVEYOR_BUILD_VERSION%-%SHA%.zip
ren IronScheme-latest-DEBUG.zip IronScheme-%APPVEYOR_BUILD_VERSION%-%SHA%-DEBUG.zip
ren IronScheme-latest.tar.xz IronScheme-%APPVEYOR_BUILD_VERSION%-%SHA%.tar.xz
copy IronScheme-%APPVEYOR_BUILD_VERSION%-%SHA%*.* %APPVEYOR_BUILD_FOLDER%
cd %APPVEYOR_BUILD_FOLDER%
@echo off