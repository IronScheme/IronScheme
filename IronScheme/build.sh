#!/bin/bash
set -e

dotnet build --disable-build-servers -tl:off -c Release ../IronScheme.BuildTools/IronScheme.Build/IronScheme.Build.csproj
dotnet build --disable-build-servers -tl:off -c Release -p:TargetFramework=net9.0 ../IronScheme.BuildTools/Setup/Setup.csproj
dotnet build --disable-build-servers -tl:off -c Release -p:IronSchemeBuildToolsTargetFramework=net9.0 $*