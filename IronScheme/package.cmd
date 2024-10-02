
rem dotnet build -tl:off -c Release ../IronScheme.BuildTools/IronScheme.Build/IronScheme.Build.csproj  -v:q

dotnet build --no-dependencies IronScheme.Console/IronScheme.Console.csproj -f net9.0 -r osx-x64 -tl:off -c Release -v:q -p:SkipWarn=CS1030;SkipWarn=CS0162
dotnet build --no-dependencies IronScheme.Console/IronScheme.Console.csproj -f net9.0 -r linux-x64 -tl:off -c Release -v:q -p:SkipWarn=CS1030;SkipWarn=CS0162

dotnet build --no-restore -tl:off -c Release -t:Package %*
exit /B %ERRORLEVEL%