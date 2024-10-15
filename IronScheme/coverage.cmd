@echo off

dotnet tool install -g dotnet-coverage --prerelease >nul
rem dotnet tool install -g dotnet-reportgenerator-globaltool >nul

dotnet-coverage collect -ll Verbose -f coverage "cov-run.cmd"