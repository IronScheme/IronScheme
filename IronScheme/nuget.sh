#!/bin/bash
set -e

(cd IronScheme.Console/bin/Release/net20/install-stage/IronScheme; echo '(import (nuget)) (fuck)' | dotnet IronScheme.ConsoleCore.dll);

cd IronScheme.Nuget
dotnet pack -tl:off -p:PackageName=Tool $*
dotnet pack -tl:off -p:PackageName=Core $*
cd ..

(cd IronScheme.Console/bin/Release/net20/install-stage/IronScheme; echo '(import (nuget)) (unfuck)' | dotnet IronScheme.ConsoleCore.dll);

dotnet tool install -g IronScheme.Tool --source bin/ --prerelease --allow-downgrade
echo '(ironscheme-test)' | ironscheme
echo '(compile-system-libraries)' | ironscheme
echo '(compile-system-libraries)' | ironscheme