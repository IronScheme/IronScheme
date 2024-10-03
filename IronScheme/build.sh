#!/bin/bash
set -e

dotnet build -tl:off -m -c Release ../IronScheme.BuildTools/IronScheme.Build/IronScheme.Build.csproj
dotnet build -tl:off -m -c Release -p:TargetFramework=net9.0 ../IronScheme.BuildTools/Setup/Setup.csproj
dotnet build -tl:off -m -c Release -p:IronSchemeBuildToolsTargetFramework=net9.0 $*