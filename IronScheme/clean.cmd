@msbuild /nologo IronSchemeCore.sln /m /v:m /t:Clean
@pushd IronScheme.Console\bin\
@del /s /q *.dll
@popd