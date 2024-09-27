#!/bin/bash
dotnet build -v:m -tl:off -c Release -p:IronSchemeBuildToolsTargetFramework=net9.0 $*