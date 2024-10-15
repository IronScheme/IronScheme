@echo off
setlocal
@rem dotnet build -tl:off -c Release ../IronScheme.BuildTools/IronScheme.Build/IronScheme.Build.csproj  -v:q
dotnet tool install -g dotnet-ilrepack >nul

dotnet build --no-restore -m -tl:off -c Release -clp:NoSummary -t:Package -bl:package.binlog %*
