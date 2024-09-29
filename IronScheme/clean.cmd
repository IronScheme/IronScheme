@echo off

msbuild /nologo IronSchemeCore.sln /m /v:m /t:Clean /p:NoWarn="NETSDK1138;NU1702;NETSDK1057" /p:Configuration=Release

pushd IronScheme.Console\bin\
del /s /q *.dll
del /s /q *.pdb
del /s /q *.output
popd

rd /q /s ..\IronScheme.BuildTools\Setup\bin
rd /q /s ..\IronScheme.BuildTools\Setup\obj
rd /q /s IronScheme.Console\obj