@echo off
setlocal

cd IronScheme.Console\bin\Release\net20
del /q IronScheme.FrameworkPAL.* >nul
del /q IronScheme.Console.* >nul

copy ..\netcoreapp2.1\IronScheme.ConsoleCore.dll . >nul
copy ..\net9.0\IronScheme.ConsoleCore.runtimeconfig.json . >nul

echo (compile-system-libraries) | dotnet IronScheme.ConsoleCore.dll -debug