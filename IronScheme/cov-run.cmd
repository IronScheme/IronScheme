@echo off
setlocal

cd IronScheme.Console\bin\Release\net20
del /q IronScheme.FrameworkPAL.*

copy ..\netcoreapp2.1\IronScheme.ConsoleCore.dll .
copy ..\net9.0\IronScheme.ConsoleCore.runtimeconfig.json .

echo (compile-system-libraries) | dotnet IronScheme.ConsoleCore.dll -debug

SET ISWD=%CD%
SET TESTCORE=1

SET LOGGER=--logger GitHubActions;summary.includePassedTests=true;summary.includeSkippedTests=true

dotnet test -v n ..\..\..\..\IronScheme.Tests\bin\Release\IronScheme.Tests.dll %LOGGER% --filter "Category=Conformance|SRFI|Other" -- NUnit.DefaultTestNamePattern="{c}.{m}" NUnit.PreFilter=true NUnit.StopOnError=false
dotnet test -v n ..\..\..\..\IronScheme.Tests\bin\Release\IronScheme.Tests.dll %LOGGER% --filter "Bootstrap_Debug" -- NUnit.DefaultTestNamePattern="{c}.{m}" NUnit.PreFilter=true NUnit.StopOnError=false