#!/bin/bash
set -e

dotnet tool install -g dotnet-ilverify

dotnet build IronScheme.Tests/IronScheme.Tests.csproj -restore -tl:off -c Release -p:TargetFramework=net9.0 --disable-build-servers --no-incremental --force
dotnet build IronScheme.Tests/IronScheme.Tests.csproj -restore -tl:off -c Release -p:TargetFramework=net9.0 --disable-build-servers --no-incremental --force

cd IronScheme.Console/bin/Release/net9.0/
export ISWD=$PWD
export TESTCORE=1

dotnet test -v d ../../../../IronScheme.Tests/bin/Release/IronScheme.Tests.dll -- NUnit.DefaultTestNamePattern="{c}.{m}" NUnit.PreFilter=true NUnit.StopOnError=true
cd ../../../..