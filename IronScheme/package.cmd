@echo off
setlocal

dotnet tool install -g dotnet-ilrepack >nul

endLocal & goto #_undefined_# 2>NUL || dotnet build --no-restore -m -tl:off -c Release -t:Package -bl:package.binlog %*
