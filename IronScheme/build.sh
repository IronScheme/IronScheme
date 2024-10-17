#!/bin/bash
set -e

COMMON='-m -c Release -clp:NoSummary'

dotnet build $COMMON -p:TargetFramework=netstandard2.0 ../IronScheme.BuildTools/IronScheme.Build/IronScheme.Build.csproj
dotnet build $COMMON -p:TargetFramework=net9.0 ../IronScheme.BuildTools/Setup/Setup.csproj
dotnet build $COMMON -bl:build.binlog -p:IronSchemeBuildToolsTargetFramework=net9.0 $*