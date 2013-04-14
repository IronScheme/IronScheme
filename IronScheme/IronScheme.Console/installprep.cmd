@call ilmerge-dll.cmd
..\..\..\tools\NamespaceRenamer IronScheme.dll Microsoft=IronScheme Oyster.Math=IronScheme.Scripting.Math gppg=IronScheme.gppg
..\..\..\tools\ReferenceRemover IronScheme.Web.Runtime.dll "IronScheme\..*|Oyster.IntX.*" IronScheme.dll
..\..\..\tools\ReferenceRemover IronScheme.Remoting.Server.dll "IronScheme\..*|Oyster.IntX.*" "IronScheme.Remoting.*" IronScheme.dll
..\..\..\tools\NamespaceRenamer -r IronScheme.Web.Runtime.dll Microsoft=IronScheme
..\..\..\tools\NamespaceRenamer -r IronScheme.Remoting.Server.dll Microsoft=IronScheme
peverify /nologo /ignore=0x80131820,0x801318DE,0x80131854,0x8013185D,0x80131228 IronScheme.dll
@IF %ERRORLEVEL% NEQ 0 exit /b 1
peverify /nologo IronScheme.Web.Runtime.dll
@IF %ERRORLEVEL% NEQ 0 exit /b 1
peverify /nologo IronScheme.Remoting.Server.dll
@IF %ERRORLEVEL% NEQ 0 exit /b 1
@call genv4.cmd
@call stage.cmd