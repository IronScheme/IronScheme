#!/bin/bash
set -e

COMMON='-m -c Release -clp:NoSummary'

dotnet build $COMMON ../IronScheme.BuildTools/IronScheme.Build/IronScheme.Build.csproj -v:q
dotnet build $COMMON -p:TargetFramework=net9.0 ../IronScheme.BuildTools/Setup/Setup.csproj -v:q
dotnet build $COMMON -bl:build.binlog -p:IronSchemeBuildToolsTargetFramework=net9.0 $*