#!/bin/bash
dotnet build -tl:off -c Release -p:TargetFramework=net9.0 ../IronScheme.BuildTools/IronScheme.BuildTools.sln
dotnet build -tl:off -c Release -p:IronSchemeBuildToolsTargetFramework=net9.0 $*