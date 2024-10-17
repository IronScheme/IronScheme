#!/bin/bash
set -e

cd IronScheme.Nuget

dotnet pack -tl:off -p:PackageName=Tool $*
dotnet pack -tl:off -p:PackageName=Core $*