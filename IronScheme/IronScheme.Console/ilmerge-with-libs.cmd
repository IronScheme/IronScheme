c:\Windows\Microsoft.NET\Framework\v2.0.50727\csc.exe /t:library /res:ironscheme.libs /out:ironscheme.libs.dll
"C:\Program Files\Microsoft\ILMerge\ILMerge.exe"/ndebug /out:isc.exe IronScheme.Console.exe IronScheme.dll IronScheme.Closures.dll Microsoft.Scripting.dll Oyster.IntX.dll ironscheme.boot.dll ironscheme.libs.dll 
del ironscheme.libs.dll