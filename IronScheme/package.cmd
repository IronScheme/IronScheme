
dotnet build --disable-build-servers -tl:off -c Release ../IronScheme.BuildTools/IronScheme.Build/IronScheme.Build.csproj
dotnet build --disable-build-servers -tl:off -c Release -t:Package %*
exit /B %ERRORLEVEL%