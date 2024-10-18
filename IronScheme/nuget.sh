#!/bin/bash
set -e

pushd IronScheme.Console/bin/Release/net20/install-stage/IronScheme
echo '(import (nuget)) (fuck)' | dotnet IronScheme.ConsoleCore.dll
popd 

pushd IronScheme.Nuget
dotnet pack -tl:off -p:PackageName=Tool $*
dotnet pack -tl:off -p:PackageName=Core $*
popd

pushd IronScheme.Console/bin/Release/net20/install-stage/IronScheme
echo '(import (nuget)) (unfuck)' | dotnet IronScheme.ConsoleCore.dll
popd 