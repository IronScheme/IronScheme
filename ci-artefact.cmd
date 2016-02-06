cd IronScheme
call package.cmd
ren bin\IronScheme-latest.zip ..\IronScheme-%APPVEYOR_BUILD_VERSION%-%SHA%.zip
ren bin\IronScheme-latest.tar.xz ..\IronScheme-%APPVEYOR_BUILD_VERSION%-%SHA%.tar.xz
cd ..