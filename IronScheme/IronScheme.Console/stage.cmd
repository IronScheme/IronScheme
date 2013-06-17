rmdir /s /q install-stage
mkdir install-stage\IronScheme

copy IronScheme.Console.exe install-stage\IronScheme\IronScheme.Console-v2.exe
copy IronScheme.Console32.exe install-stage\IronScheme\IronScheme.Console32-v2.exe
copy IronScheme.Console-v4.exe install-stage\IronScheme
copy IronScheme.Console32-v4.exe install-stage\IronScheme
copy DEVELOPMENT.snk install-stage\IronScheme
copy system-libraries.ss install-stage\IronScheme


copy IronScheme.dll install-stage\IronScheme
copy IronScheme.Web.Runtime.dll install-stage\IronScheme
copy IronScheme.Remoting*.dll install-stage\IronScheme
copy ExecutableTemplate.cs install-stage\IronScheme

xcopy /e ..\..\examples install-stage\IronScheme\examples\
xcopy /e ..\..\docs install-stage\IronScheme\docs\
xcopy /e ..\..\lib install-stage\IronScheme\lib\
xcopy /e ..\..\tests install-stage\IronScheme\tests\

del install-stage\IronScheme\lib\minikanren\mktests.scm
del install-stage\IronScheme\lib\pfds\tests.scm
del install-stage\IronScheme\tests\*.sps
del install-stage\IronScheme\lib\wak\fmt\private\test*.scm
del install-stage\IronScheme\lib\wak\foof-loop\private\test-foof-loop.scm
rmdir /s /q install-stage\IronScheme\lib\srfi\tests
rmdir /s /q install-stage\IronScheme\docs\notes

cd install-stage
c:\MinGW\msys\1.0\bin\tar.exe -c -fIronScheme-latest.tar IronScheme
..\xz IronScheme-latest.tar

copy ngen-all.cmd install-stage\IronScheme
copy un-ngen-all.cmd install-stage\IronScheme

..\zip -r -9 IronScheme-latest.zip IronScheme
cd ..
