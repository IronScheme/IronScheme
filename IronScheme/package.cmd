@echo off
setlocal
rem dotnet build -tl:off -c Release ../IronScheme.BuildTools/IronScheme.Build/IronScheme.Build.csproj  -v:q
dotnet tool install -g dotnet-ilrepack >nul

endLocal & goto #_undefined_# 2>NUL || dotnet build --no-restore -m -tl:off -c Release -clp:NoSummary -t:Package -bl:package.binlog %*
