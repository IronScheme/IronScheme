@echo off

setlocal

dotnet tool install -g dotnet-coverage --prerelease >nul
dotnet tool install -g dotnet-ilverify --prerelease >nul
rem dotnet tool install -g dotnet-reportgenerator-globaltool >nul

del /q output.cobertura.xml
del /q *.coverage
del /q coverage.xml

rem endLocal & goto #_undefined_# 2>NUL ||

dotnet-coverage collect "cov-compile.cmd" -o compile.coverage
dotnet-coverage collect "cov-run.cmd" -o run.coverage

rem dotnet-coverage merge compile.coverage run.coverage -f xml -o coverage.xml
rem reportgenerator -reports:coverage.xml -targetdir:cov