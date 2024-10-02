
dotnet build --disable-build-servers -tl:off -c Release ../IronScheme.BuildTools/IronScheme.Build/IronScheme.Build.csproj

dotnet build --disable-build-servers IronScheme.Console/IronScheme.Console.csproj -f net9.0 -r osx-x64 -tl:off -v:n -c Release
dotnet build --disable-build-servers IronScheme.Console/IronScheme.Console.csproj -f net9.0 -r linux-x64 -tl:off -v:n -c Release

dotnet build --disable-build-servers -tl:off -c Release -t:Package %*
exit /B %ERRORLEVEL%