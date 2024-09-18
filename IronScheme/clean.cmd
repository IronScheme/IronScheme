@msbuild /nologo IronSchemeCore.sln /m /v:m /t:Clean /p:NoWarn="NETSDK1138;NU1702"
@pushd IronScheme.Console\bin\
@del /s /q *.dll
@popd