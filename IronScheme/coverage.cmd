@echo off

dotnet tool install -g dotnet-coverage --prerelease >nul
dotnet tool install -g dotnet-ilverify --prerelease >nul
rem dotnet tool install -g dotnet-reportgenerator-globaltool >nul

dotnet-coverage collect "cov-run.cmd"