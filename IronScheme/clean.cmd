@echo off

msbuild /nologo IronSchemeCore.sln /m /v:m /t:Clean /p:NoWarn="NETSDK1138;NU1702"

pushd IronScheme.Console\bin\
del /s /q *.dll
popd

rd /q /s ..\IronScheme.BuildTools\Setup\bin
rd /q /s ..\IronScheme.BuildTools\Setup\obj
rd /q /s IronScheme.Console\obj