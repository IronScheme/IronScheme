@echo off
setlocal

pushd IronScheme.Console\bin\Release\net20\install-stage\IronScheme
echo (import (nuget)) (fuck) | IronScheme.Console32-v4
popd

pushd IronScheme.Nuget
dotnet pack -tl:off -p:PackageName=Tool %*
dotnet pack -tl:off -p:PackageName=Core %*
popd

pushd IronScheme.Console\bin\Release\net20\install-stage\IronScheme
echo (import (nuget)) (unfuck) | IronScheme.Console32-v4
popd

dotnet tool install -g IronScheme.Tool --source bin/ --prerelease --allow-downgrade
echo (ironscheme-test) | ironscheme
echo (compile-system-libraries) | ironscheme
echo (compile-system-libraries) | ironscheme