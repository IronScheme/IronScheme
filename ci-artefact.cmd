@call IronScheme\package.cmd
@ren IronScheme\bin\IronScheme-latest.zip IronScheme-%APPVEYOR_BUILD_VERSION%-%SHA%.zip
@ren IronScheme\bin\IronScheme-latest.tar.xz IronScheme-%APPVEYOR_BUILD_VERSION%-%SHA%.tar.xz