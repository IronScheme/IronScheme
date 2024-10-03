@echo off
setlocal

@SET COMMON=-tl:off -m -c Release -v:q

dotnet clean %COMMON% -nowarn:NETSDK1138,NU1702,NETSDK1057

pushd IronScheme.Console\bin\
del /s /q *.dll >nul 2>&1
del /s /q *.pdb >nul 2>&1
del /s /q *.output >nul 2>&1
popd

rd /q /s ..\IronScheme.BuildTools\Setup\bin >nul 2>&1
rd /q /s ..\IronScheme.BuildTools\Setup\obj >nul 2>&1
rd /q /s IronScheme.Console\obj >nul 2>&1