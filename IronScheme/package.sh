#!/bin/bash
set -e

dotnet tool install -g dotnet-ilrepack

dotnet build --disable-build-servers -tl:off -c Release ../IronScheme.BuildTools/IronScheme.Build/IronScheme.Build.csproj
dotnet build --disable-build-servers -tl:off -c Release -t:Package $*