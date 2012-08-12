rmdir /s /q install-stage
mkdir install-stage\IronScheme

copy IronScheme.Console.exe install-stage\IronScheme\IronScheme.Console-v2.exe
copy IronScheme.Console32.exe install-stage\IronScheme\IronScheme.Console32-v2.exe
copy IronScheme.Console-v4.exe install-stage\IronScheme
copy IronScheme.Console32-v4.exe install-stage\IronScheme
copy DEVELOPMENT.snk install-stage\IronScheme
copy system-libraries.ss install-stage\IronScheme

copy ngen-all.cmd install-stage\IronScheme
copy IronScheme.dll install-stage\IronScheme
copy IronScheme.Web.Runtime.dll install-stage\IronScheme
copy IronScheme.Remoting*.dll install-stage\IronScheme

xcopy /e ..\..\examples install-stage\IronScheme\examples\
xcopy /e ..\..\docs install-stage\IronScheme\docs\
xcopy /e ..\..\ironscheme install-stage\IronScheme\ironscheme\
xcopy /e ..\..\srfi install-stage\IronScheme\srfi\
xcopy /e ..\..\lib install-stage\IronScheme\lib\

cd install-stage
..\zip -r -9 IronScheme-latest.zip IronScheme
..\tar -c -fIronScheme-latest.tar IronScheme
..\xz IronScheme-latest.tar
cd ..
