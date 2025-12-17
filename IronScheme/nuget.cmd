@echo off
setlocal

pushd IronScheme.Console\bin\Release\net20\install-stage\IronScheme
echo (import (nuget)) (fuck) | IronScheme.Console32-v4
popd

pushd IronScheme.Console\bin\Release\net20\install-stage
mkdir tmp\build\
mkdir tmp\contentFiles\any\any\
mklink /j tmp\contentFiles\any\any\lib IronScheme\lib\
mklink /h tmp\contentFiles\any\any\nuget.sls IronScheme\nuget.sls
popd

pushd IronScheme.Nuget
copy /y IronScheme.targets ..\IronScheme.Console\bin\Release\net20\install-stage\tmp\build\
dotnet pack -tl:off -p:PackageName=Core %*
dotnet pack -tl:off %*
popd

pushd IronScheme.Nuget
dotnet pack -tl:off -p:PackageName=Tool %*
popd

pushd IronScheme.Console\bin\Release\net20\install-stage\IronScheme
echo (import (nuget)) (unfuck) | IronScheme.Console32-v4
popd

dotnet tool install -g IronScheme.Tool --source bin/ --prerelease --allow-downgrade
echo (ironscheme-test) | ironscheme
echo (compile-system-libraries) | ironscheme
echo (compile-system-libraries) | ironscheme