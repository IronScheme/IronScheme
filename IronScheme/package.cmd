rem @echo off
setlocal

set BUILD_ROOT=%~dp0
set PATH=%PATH%;%BUILD_ROOT%tools;

if "%APPVEYOR_BUILD_VERSION%"=="" set APPVEYOR_BUILD_VERSION="0.0.0"
if "%SHA%"=="" set SHA="000000"

mkdir bin >nul 2>&1

pushd IronScheme.Console\bin\Release\net20

::echo 'starting log' > package.log
rmdir /s /q merged
mkdir merged >nul 2>&1

IronScheme.Console.exe -debug ironscheme-buildscript.sps >nul 2>&1
echo '1' | IronScheme.Console.exe -debug

set MERGE=ilrepack

ReferenceRemover IronScheme.FrameworkPAL.dll "IronScheme\.Scripting" IronScheme.dll >nul
ReferenceRemover ..\netcoreapp2.1\IronScheme.FrameworkPAL.dll "IronScheme\.Scripting" IronScheme.dll >nul
ReferenceRemover ..\net9.0\IronScheme.FrameworkPAL.dll "IronScheme\.Scripting" IronScheme.dll >nul

NamespaceRenamer -r IronScheme.FrameworkPAL.dll Microsoft=IronScheme >nul
NamespaceRenamer -r ..\netcoreapp2.1\IronScheme.FrameworkPAL.dll Microsoft=IronScheme >nul
NamespaceRenamer -r ..\net9.0\IronScheme.FrameworkPAL.dll Microsoft=IronScheme >nul

%MERGE% /out:..\net9.0\net9-IronScheme.FrameworkPAL.dll /noRepackRes ..\net9.0\IronScheme.FrameworkPAL.dll ..\net9.0\System.Runtime.Serialization.Formatters.dll

al /out:IronScheme.PALResources.dll /embed:IronScheme.FrameworkPAL.dll /embed:"..\netcoreapp2.1\IronScheme.FrameworkPAL.dll",core-IronScheme.FrameworkPAL.dll /embed:"..\net9.0\net9-IronScheme.FrameworkPAL.dll"

%MERGE% /out:merged\IronScheme.dll IronScheme.dll IronScheme.Closures.dll IronScheme.Scripting.dll Oyster.IntX.dll ironscheme.boot.dll IronScheme.PALResources.dll
copy /Y merged\IronScheme.* . >nul
peverify /nologo /ignore=0x80131820,0x801318DE,0x80131854,0x8013185D,0x80131228 IronScheme.dll >nul

NamespaceRenamer IronScheme.dll Microsoft=IronScheme Oyster.Math=IronScheme.Scripting.Math gppg=IronScheme.gppg >nul

peverify /nologo /ignore=0x80131820,0x801318DE,0x80131854,0x8013185D,0x80131228 IronScheme.dll >nul

rem ilrepack cant seem to deal with v4
ILMerge /ndebug /v4 /out:IronScheme.Console-v4.exe IronScheme.Console.exe
ILMerge /ndebug /v4 /out:IronScheme.Console32-v4.exe IronScheme.Console32.exe
rem the monolith IronScheme.dll is now built, start with packaging

mkdir install-stage >nul 2>&1
rmdir /s /q install-stage
mkdir install-stage\IronScheme

copy IronScheme.Console.exe install-stage\IronScheme\IronScheme.Console-v2.exe
copy IronScheme.Console32.exe install-stage\IronScheme\IronScheme.Console32-v2.exe
copy IronScheme.Console-v4.exe install-stage\IronScheme
copy IronScheme.Console32-v4.exe install-stage\IronScheme

copy ..\netcoreapp2.1\IronScheme.ConsoleCore.dll install-stage\IronScheme
copy ..\netcoreapp2.1\IronScheme.ConsoleCore.pdb install-stage\IronScheme
copy ..\netcoreapp2.1\IronScheme.ConsoleCore.runtimeconfig.json install-stage\IronScheme

ReferenceRemover install-stage\IronScheme\IronScheme.ConsoleCore.dll "IronScheme\.Closures" IronScheme.dll >nul

copy ..\net9.0\IronScheme.ConsoleCore.exe install-stage\IronScheme

copy system-libraries.ss install-stage\IronScheme
copy system-libraries.srfi.ss install-stage\IronScheme

copy IronScheme.dll install-stage\IronScheme
xcopy /e ..\..\..\examples install-stage\IronScheme\examples\ >nul
xcopy /e ..\..\..\docs install-stage\IronScheme\docs\ >nul
xcopy /e ..\..\..\lib install-stage\IronScheme\lib\ >nul
xcopy /e ..\..\..\tests install-stage\IronScheme\tests\ >nul
del install-stage\IronScheme\lib\minikanren\mktests.scm
del install-stage\IronScheme\lib\pfds\tests.scm
del install-stage\IronScheme\tests\*.sps
del install-stage\IronScheme\lib\wak\fmt\private\test*.scm
del install-stage\IronScheme\lib\wak\foof-loop\private\test-foof-loop.scm
del install-stage\IronScheme\IronScheme.ConsoleCore.pdb
rmdir /s /q install-stage\IronScheme\lib\srfi\tests
rmdir /s /q install-stage\IronScheme\docs\notes
cd install-stage
copy ..\ngen-all.cmd IronScheme
copy ..\un-ngen-all.cmd IronScheme
7z a IronScheme-latest.zip IronScheme
copy ..\IronScheme.pdb IronScheme
7z a IronScheme-latest-DEBUG.zip IronScheme
rem rename artefacts and copy to build root
copy /y IronScheme-latest*.* %BUILD_ROOT%bin
rem nuget
copy /y ..\..\..\..\IronScheme.Core.nuspec .
nuget pack IronScheme.Core.nuspec -properties version=%APPVEYOR_BUILD_VERSION%;sha=%SHA%
rem nuget pack IronScheme.Core.nuspec -Symbols -SymbolPackageFormat snupkg -properties version=%APPVEYOR_BUILD_VERSION%;sha=%SHA%
copy /y IronScheme*.*nupkg %BUILD_ROOT%bin
popd
