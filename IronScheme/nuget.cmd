@echo off
setlocal

cd IronScheme.Nuget

dotnet pack -tl:off -p:PackageName=Tool %*
dotnet pack -tl:off -p:PackageName=Core %*