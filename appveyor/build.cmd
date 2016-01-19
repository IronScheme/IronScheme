appveyor AddCompilationMessage "Starting build"
msbuild "IronScheme\IronSchemeCI.sln" /verbosity:minimal /logger:"C:\Program Files\AppVeyor\BuildAgent\Appveyor.MSBuildLogger.dll" /p:TargetFrameworkVersion=v2.0 /p:Configuration=Release
appveyor AddCompilationMessage "Build completed"