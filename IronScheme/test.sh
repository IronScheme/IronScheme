#!/bin/bash
set -e

dotnet tool install -g dotnet-ilverify --prerelease

dotnet build IronScheme.Tests/IronScheme.Tests.csproj -restore -m -tl:off -c Release -p:TargetFramework=net9.0 --no-incremental --force -v:q
dotnet build IronScheme.Tests/IronScheme.Tests.csproj -restore -m -tl:off -c Release -p:TargetFramework=net9.0 --no-incremental --force -v:q

cd IronScheme.Console/bin/Release/net9.0/
export ISWD=$PWD
export TESTCORE=1

LOGGER=--logger "GitHubActions;summary.includePassedTests=true;summary.includeSkippedTests=true"

dotnet test -v d ../../../../IronScheme.Tests/bin/Release/IronScheme.Tests.dll $LOGGER $* -- NUnit.DefaultTestNamePattern="{c}.{m}" NUnit.PreFilter=true NUnit.StopOnError=false
cd ../../../..