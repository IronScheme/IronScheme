@echo off

setlocal

dotnet tool install -g dotnet-coverage --prerelease >nul
dotnet tool install -g dotnet-ilverify --prerelease >nul
rem dotnet tool install -g dotnet-reportgenerator-globaltool >nul

del /q output.cobertura.xml
del /q output.coverage

endLocal & goto #_undefined_# 2>NUL || dotnet-coverage collect "cov-run.cmd"

rem dotnet-coverage merge output.coverage -f cobertura
rem reportgenerator -reports:output.cobertura.xml -targetdir:cov