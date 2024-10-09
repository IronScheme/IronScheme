@echo off
setlocal

dotnet build-server shutdown --msbuild >nul

@SET COMMON=-m -c Release -clp:NoSummary -v:q

rem dotnet clean ../IronScheme.BuildTools/ %COMMON% -nowarn:NETSDK1138,NU1702,NETSDK1057
dotnet clean %COMMON% -nowarn:NETSDK1138,NU1702,NETSDK1057 %*

pushd IronScheme.Console\bin\
del /s /q *.dll >nul 2>&1
del /s /q *.pdb >nul 2>&1
del /s /q *.output >nul 2>&1
popd

rd /q /s ..\IronScheme.BuildTools\Setup\bin >nul 2>&1
rd /q /s ..\IronScheme.BuildTools\Setup\obj >nul 2>&1
rd /q /s IronScheme.Console\obj >nul 2>&1