#!/bin/bash
dotnet build --disable-build-servers -restore -v:m -tl:off -c Release -p:TargetFramework=net9.0 IronScheme.Tests/IronScheme.Tests.csproj

cd IronScheme.Console/bin/Release/net9.0/
export ISWD=$PWD
export TESTCORE=1

dotnet test -v n ../../../../IronScheme.Tests/bin/Release/IronScheme.Tests.dll -- NUnit.DefaultTestNamePattern="{c}.{m}" NUnit.PreFilter=true NUnit.StopOnError=true
cd ../../../..