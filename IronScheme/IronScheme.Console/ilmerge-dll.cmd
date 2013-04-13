"C:\Program Files (x86)\Microsoft\ILMerge\ILMerge.exe" /ndebug /keyfile:DEVELOPMENT.snk /out:merged\IronScheme.dll IronScheme.dll IronScheme.Closures.dll IronScheme.Scripting.dll Oyster.IntX.dll ironscheme.boot.dll 
copy /Y merged\IronScheme.* .
peverify /nologo /ignore=0x80131820,0x801318DE,0x80131854,0x8013185D,0x80131228 IronScheme.dll
@IF %ERRORLEVEL% NEQ 0 exit /b 1