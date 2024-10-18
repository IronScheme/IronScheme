#!/bin/bash
set -e

dotnet tool install -g dotnet-ilverify --prerelease

dotnet build IronScheme.Tests/IronScheme.Tests.csproj -restore -m -tl:off -c Release -p:TargetFramework=net9.0 --no-incremental --force -v:q -clp:NoSummary
dotnet build IronScheme.Tests/IronScheme.Tests.csproj -restore -m -tl:off -c Release -p:TargetFramework=net9.0 --no-incremental --force -v:q -clp:NoSummary

cd IronScheme.Console/bin/Release/netcoreapp2.1/
export ISWD=$PWD
export TESTCORE=1

FILTER='--filter "Category=Conformance|SRFI|Other"'
LOGGER='--logger GitHubActions;summary.includePassedTests=true;summary.includeSkippedTests=true'

dotnet test -v d ../../../../IronScheme.Tests/bin/Release/IronScheme.Tests.dll $LOGGER $FILTER $* -- NUnit.DefaultTestNamePattern="{c}.{m}" NUnit.PreFilter=true NUnit.StopOnError=false