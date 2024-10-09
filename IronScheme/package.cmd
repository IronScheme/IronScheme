@echo off
setlocal
@rem dotnet build -tl:off -c Release ../IronScheme.BuildTools/IronScheme.Build/IronScheme.Build.csproj  -v:q
dotnet tool install -g dotnet-ilrepack >nul

@SET COMMON=--no-dependencies -tl:off -m -c Release IronScheme.Console/IronScheme.Console.csproj -f net9.0 -v:q -noWarn:CS1030,CS0162 -clp:NoSummary

dotnet build %COMMON% -r osx-x64
dotnet build %COMMON% -r linux-x64

dotnet build --no-restore -m -tl:off -c Release -clp:NoSummary -t:Package -bl:package.binlog %*
