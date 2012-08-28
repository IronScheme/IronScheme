call ilmerge-dll.cmd

..\..\..\tools\ReferenceRemover IronScheme.Web.Runtime.dll "IronScheme\..*|Oyster.IntX.*" IronScheme.dll
..\..\..\tools\ReferenceRemover IronScheme.Remoting.Server.dll "IronScheme\..*|Oyster.IntX.*" "IronScheme.Remoting.*" IronScheme.dll
call genv4.cmd
call stage.cmd