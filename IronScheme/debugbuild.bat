%windir%\microsoft.net\framework\v3.5\msbuild IronScheme2008.sln /p:Configuration=Debug "/p:Platform=Any CPU"
@IF %ERRORLEVEL% NEQ 0 GOTO err
@exit /B 0
:err
@PAUSE
@exit /B 1