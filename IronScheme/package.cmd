@echo off
setlocal

set BUILD_ROOT=%~dp0
set PATH=%PATH%;%BUILD_ROOT%tools;

mkdir bin >nul 2>&1

pushd IronScheme.Console\bin\Release\

::echo 'starting log' > package.log
mkdir merged >nul 2>&1

IronScheme.Console32.exe -debug ironscheme-buildscript.sps >nul 2>&1
echo '1' | IronScheme.Console32.exe -debug

ILMerge /keyfile:DEVELOPMENT.snk /out:merged\IronScheme.dll IronScheme.dll IronScheme.Closures.dll IronScheme.Scripting.dll Oyster.IntX.dll ironscheme.boot.dll 
copy /Y merged\IronScheme.* . >nul
peverify /nologo /ignore=0x80131820,0x801318DE,0x80131854,0x8013185D,0x80131228 IronScheme.dll >nul

NamespaceRenamer IronScheme.dll Microsoft=IronScheme Oyster.Math=IronScheme.Scripting.Math gppg=IronScheme.gppg >nul
ReferenceRemover IronScheme.Web.Runtime.dll "IronScheme\..*|Oyster.IntX.*" IronScheme.dll >nul
ReferenceRemover IronScheme.Remoting.Server.dll "IronScheme\..*|Oyster.IntX.*" "IronScheme.Remoting.*" IronScheme.dll >nul
NamespaceRenamer -r IronScheme.Web.Runtime.dll Microsoft=IronScheme >nul
NamespaceRenamer -r IronScheme.Remoting.Server.dll Microsoft=IronScheme >nul

peverify /nologo /ignore=0x80131820,0x801318DE,0x80131854,0x8013185D,0x80131228 IronScheme.dll >nul
peverify /nologo IronScheme.Web.Runtime.dll >nul
peverify /nologo IronScheme.Remoting.Server.dll >nul

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
copy DEVELOPMENT.snk install-stage\IronScheme
copy system-libraries.ss install-stage\IronScheme
copy system-libraries.srfi.ss install-stage\IronScheme
copy IronScheme.dll install-stage\IronScheme
copy IronScheme.Web.Runtime.dll install-stage\IronScheme
copy IronScheme.Remoting*.dll install-stage\IronScheme >nul
copy Executable.cs.template install-stage\IronScheme
xcopy /e ..\..\examples install-stage\IronScheme\examples\ >nul
xcopy /e ..\..\docs install-stage\IronScheme\docs\ >nul
xcopy /e ..\..\lib install-stage\IronScheme\lib\ >nul
xcopy /e ..\..\tests install-stage\IronScheme\tests\ >nul
del install-stage\IronScheme\lib\minikanren\mktests.scm
del install-stage\IronScheme\lib\pfds\tests.scm
del install-stage\IronScheme\tests\*.sps
del install-stage\IronScheme\lib\wak\fmt\private\test*.scm
del install-stage\IronScheme\lib\wak\foof-loop\private\test-foof-loop.scm
rmdir /s /q install-stage\IronScheme\lib\srfi\tests
rmdir /s /q install-stage\IronScheme\docs\notes
cd install-stage
tar -c -fIronScheme-latest.tar IronScheme >nul
xz IronScheme-latest.tar
copy ..\IronScheme.pdb IronScheme
copy ..\ngen-all.cmd IronScheme
copy ..\un-ngen-all.cmd IronScheme
zip -r -9 -q IronScheme-latest.zip IronScheme
rem rename artefacts and copy to build root
copy /y IronScheme-latest.* %BUILD_ROOT%bin
popd