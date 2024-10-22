@echo off
setlocal

dotnet build-server shutdown --msbuild >nul
\sysinternals\pskill -nobanner msbuild 2>&1

@SET COMMON=-m -c Release -clp:NoSummary -nowarn:NETSDK1138,NU1702,NETSDK1057 --disable-build-servers

rem dotnet clean ../IronScheme.BuildTools/ %COMMON% -nowarn:NETSDK1138,NU1702,NETSDK1057
dotnet clean %COMMON% %*

pushd IronScheme.Console\bin\
del /s /q *.dll >nul 2>&1
del /s /q *.pdb >nul 2>&1
del /s /q *.output >nul 2>&1
popd

rd /q /s IronScheme.Console\obj >nul 2>&1