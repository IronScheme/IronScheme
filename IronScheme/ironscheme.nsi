

!macro NET2
!macroend

!ifmacrodef NET4
!define NETVERSION "NET4"
!else
!define NETVERSION "NET2"
!endif

!include "x64.nsh"

!define PRODUCT_NAME "IronScheme"
!define PRODUCT_VERSION "1.0-RC7"
!define PRODUCT_PUBLISHER "leppie"
!define PRODUCT_WEB_SITE "http://ironscheme.codeplex.com"
!define PRODUCT_DIR_REGKEY "Software\Microsoft\Windows\CurrentVersion\App Paths\IronScheme.Console.exe"
!define PRODUCT_UNINST_KEY "Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRODUCT_NAME}"
!define PRODUCT_UNINST_ROOT_KEY "HKLM"

SetCompressor /SOLID lzma
XPStyle on

Name "${PRODUCT_NAME} ${PRODUCT_VERSION} ${NETVERSION}"
OutFile "IronScheme-${PRODUCT_VERSION}-${NETVERSION}-setup.exe"
InstallDir "$PROGRAMFILES\IronScheme"
InstallDirRegKey HKLM "${PRODUCT_DIR_REGKEY}" ""
ShowInstDetails show
ShowUnInstDetails show

!include "MUI2.nsh"

; MUI Settings
!define MUI_ABORTWARNING
!define MUI_COMPONENTSPAGE_NODESC
!define MUI_ICON "..\..\..\ironscheme.ico"
!define MUI_UNICON "..\..\..\ironscheme.ico"
!define MUI_HEADERIMAGE
!define MUI_HEADERIMAGE_RIGHT
!define MUI_HEADERIMAGE_BITMAP "..\..\..\header.bmp"
!define MUI_WELCOMEFINISHPAGE_BITMAP "..\..\..\welcome.bmp"

; Welcome page
!insertmacro MUI_PAGE_WELCOME
; Directory page
!insertmacro MUI_PAGE_DIRECTORY
; Instfiles page
!insertmacro MUI_PAGE_INSTFILES
; Finish page
!insertmacro MUI_PAGE_FINISH

; Uninstaller pages
!insertmacro MUI_UNPAGE_INSTFILES

; Language files
!insertmacro MUI_LANGUAGE "English"

; MUI end ------

Var NETPATH
var NETROOT

; IsDotNETInstalled
;
; Usage:
;   Call IsDotNETInstalled
;   Pop $0
;   StrCmp $0 1 found.NETFramework no.NETFramework

Function IsDotNETInstalled
   Push $0
   Push $1
   Push $2
   Push $3
   Push $4

   ReadRegStr $4 HKEY_LOCAL_MACHINE "Software\Microsoft\.NETFramework" "InstallRoot"
   # remove trailing back slash
   Push $4
   Exch $EXEDIR
   Exch $EXEDIR
   Pop $4
   # if the root directory doesn't exist .NET is not installed
   ;DetailPrint "InstallRoot = $4"
   IfFileExists $4 0 noDotNET

   StrCpy $0 0
   StrCpy $NETROOT $4

 EnumStart:
   EnumRegKey $2 HKEY_LOCAL_MACHINE "Software\Microsoft\.NETFramework\Policy"  $0
   IntOp $0 $0 + 1
   ;DetailPrint "Policy = $2"
   StrCmp $2 "" noDotNET
   StrCpy $1 0

 EnumPolicy:
   EnumRegValue $3 HKEY_LOCAL_MACHINE "Software\Microsoft\.NETFramework\Policy\$2" $1
   IntOp $1 $1 + 1
   ;DetailPrint "PolicyKey = $3"
   StrCmp $3 "" EnumStart
!ifmacrondef NET4
   ;DetailPrint "Check for .NET 2 @ $4\v2.0.$3"
   IfFileExists "$4\v2.0.$3" foundDotNET2 
!else
   ;DetailPrint "Check for .NET 4 @ $4\v4.0.$3"
   IfFileExists "$4\v4.0.$3" foundDotNET4 
!endif
   Goto EnumPolicy

  noDotNET:
    DetailPrint ".NET not detected."
    StrCpy $0 0
    Goto done

!ifmacrondef NET4
  foundDotNET2:
    DetailPrint ".NET 2.0 detected @ $4\v2.0.$3."
    StrCpy $0 "$4\v2.0.$3"
    Goto done
!else
  foundDotNET4:
    DetailPrint ".NET 4.0 detected @ $4\v4.0.$3."
    StrCpy $0 "$4\v4.0.$3"
    Goto done
!endif

  done:
    Pop $4
    Pop $3
    Pop $2
    Pop $1
    Exch $0
FunctionEnd

InstType "Full"
InstType "Minimal"

Function .onInit

${If} ${RunningX64}
  StrCpy $INSTDIR "$PROGRAMFILES64\IronScheme"
  SetRegView 64
${Else}
  StrCpy $INSTDIR "$PROGRAMFILES\IronScheme"
  SetRegView 32
${EndIf}
  
FunctionEnd

Section -$(SEC_DOTNET) SECDOTNET
SectionIn 1 2 RO

Call IsDotNETInstalled
Pop $NETPATH
StrCmp $NETPATH 0 PromptDownload Install

PromptDownload:
Abort  "Please install .NET first"

Install:

SectionEnd

Section "IronScheme ${PRODUCT_VERSION}" SEC01
SectionIn 1 2 RO

  SetOutPath "$INSTDIR"
  
  DetailPrint "Removing previous native images (if any)..."

  nsExec::ExecToStack '"$NETPATH\ngen.exe" uninstall "$INSTDIR\IronScheme.Console.exe"'
  nsExec::ExecToStack '"$NETPATH\ngen.exe" uninstall "$INSTDIR\ironscheme.boot.dll"'

  CreateDirectory "$SMPROGRAMS\IronScheme"

  CreateShortCut "$SMPROGRAMS\IronScheme\IronScheme.lnk" "$INSTDIR\IronScheme.Console.exe"
  CreateShortCut "$SMPROGRAMS\IronScheme\IronScheme (32-bit).lnk" "$INSTDIR\IronScheme.Console32.exe"
  CreateShortCut "$DESKTOP\IronScheme.lnk" "$INSTDIR\IronScheme.Console.exe"
  CreateShortCut "$DESKTOP\IronScheme (32-bit).lnk" "$INSTDIR\IronScheme.Console32.exe"

	File "IronScheme.Console.exe"
	File "IronScheme.Console.exe.config"
	File "IronScheme.Console32.exe"
	File "IronScheme.Console32.exe.config"	
	
	File "IronScheme.dll"
	File "IronScheme.Remoting.dll"
	File "IronScheme.Remoting.Client.dll"
	File "IronScheme.Remoting.Server.dll"
	File "IronScheme.Closures.dll"
	File "IronScheme.Web.Runtime.dll"
	
	File "ironscheme.boot.dll"
	File "Oyster.IntX.dll"
	File "IronScheme.Scripting.dll"
	File "DEVELOPMENT.snk"
	
	File "setup-xp.bat"
	File "system-libraries.ss"
	File "compile-system-libraries.sps"
	File "ExecutableTemplate.cs"
	
	File "..\..\..\IronScheme.LibraryBrowser\bin\Release\IronScheme.LibraryBrowser.exe"
	File "..\..\..\IronScheme.WebServer\bin\Release\IronScheme.WebServer.exe"
	File "..\..\..\IronScheme.WebServer\bin\Release\IronScheme.WebServer.exe.config"
	
	File ..\..\..\tools\IronScheme.VisualStudio.dll
	File ..\..\..\tools\IronScheme.VisualStudio.vsix
	File ..\..\..\tools\RegPkg.exe
	File ..\..\..\tools\RegPkg.exe.config
${If} ${RunningX64}	
  File ..\..\..\tools\IronScheme.Profiler.x64.dll
${EndIf}	
	File ..\..\..\tools\IronScheme.Profiler.x86.dll
	
	File "ironscheme-buildscript.ss"

	SetOutPath "$INSTDIR\examples"
	File /r examples\*.*
	
	SetOutPath "$INSTDIR\docs"
	File /r docs\*.txt
	
	SetOutPath "$INSTDIR\ironscheme"
	File /r ironscheme\*.sls
	
	SetOutPath "$INSTDIR\lib"
	File lib\*.sls
	
	SetOutPath "$INSTDIR\lib\wak"
	File /r lib\wak\*.sls
	File /r lib\wak\*.scm
	File lib\wak\LICENSE.*
	File lib\wak\README.*
	
	SetOutPath "$INSTDIR\lib\pfds"
	File /r lib\pfds\*.sls
	File lib\pfds\LICENSE
	File lib\pfds\README	
	
	SetOutPath "$INSTDIR\build"
	File /r build\*.ss
	
	SetOutPath "$INSTDIR\psyntax"
	File psyntax\*.ss
	
;	SetOutPath "$INSTDIR\source-optimizer"
;	File source-optimizer\*.sls
	
	SetOutPath "$INSTDIR\srfi"
	File /r srfi\*.ss
	File /r srfi\*.sps
	File /r srfi\*.sls
	File /r srfi\*.scm
	File srfi\LICENSE
	File srfi\README
	
	SetOutPath "$INSTDIR\websample"
	File ..\..\..\IronScheme.Web\test.ss
	File ..\..\..\IronScheme.Web\test2.ss
	File ..\..\..\IronScheme.Web\web.config
	File ..\..\..\IronScheme.Web\web.routes
	
	SetOutPath "$INSTDIR\websample\controllers"
	File /r ..\..\..\IronScheme.Web\controllers\*.sls
	
	SetOutPath "$INSTDIR\websample\views"
	File /r ..\..\..\IronScheme.Web\views\*.sls
	
	SetOutPath "$INSTDIR\websample\models"
	File /r ..\..\..\IronScheme.Web\models\*.sls
	
	SetOutPath "$INSTDIR\websample\styles"
	File /r ..\..\..\IronScheme.Web\styles\*.css
	
	SetOutPath "$INSTDIR\websample\data"
	File "placeholder.txt"
	
	SetOutPath "$INSTDIR\tests"
	File /r tests\*.*
	
SectionEnd


Section -AdditionalIcons
  CreateShortCut "$SMPROGRAMS\IronScheme\Uninstall.lnk" "$INSTDIR\uninstall.exe"
SectionEnd

Section -Post
  SetOutPath "$INSTDIR"
  DetailPrint "Generating native images..."

  nsExec::ExecToStack '"$NETPATH\ngen.exe" install "$INSTDIR\IronScheme.Console.exe"'
  nsExec::ExecToStack '"$NETPATH\ngen.exe" install "$INSTDIR\ironscheme.boot.dll"'

  DetailPrint "Compiling system libraries..."
  nsExec::ExecToStack '"$INSTDIR\IronScheme.Console.exe" "$INSTDIR\compile-system-libraries.sps"'
  DetailPrint "Creating symbolic links..."
  nsExec::ExecToStack 'cmd /c mklink isc.exe "$INSTDIR\IronScheme.Console.exe"'
  nsExec::ExecToStack 'cmd /c mklink isc32.exe "$INSTDIR\IronScheme.Console32.exe"'
  
  SetOutPath "$INSTDIR\websample"
  nsExec::ExecToStack 'cmd /c mkdir bin'
  SetOutPath "$INSTDIR\websample\bin"
  ; this will probably fail on non-Vista
  nsExec::ExecToStack 'cmd /c mklink IronScheme.dll "$INSTDIR\IronScheme.dll"'
  nsExec::ExecToStack 'cmd /c mklink IronScheme.Closures.dll "$INSTDIR\IronScheme.Closures.dll"'
  nsExec::ExecToStack 'cmd /c mklink Oyster.IntX.dll "$INSTDIR\Oyster.IntX.dll"'
  nsExec::ExecToStack 'cmd /c mklink ironscheme.boot.dll "$INSTDIR\ironscheme.boot.dll"'
  nsExec::ExecToStack 'cmd /c mklink Microsoft.Scripting.dll "$INSTDIR\Microsoft.Scripting.dll"'
  nsExec::ExecToStack 'cmd /c mklink IronScheme.Web.Runtime.dll "$INSTDIR\IronScheme.Web.Runtime.dll"'
  nsExec::ExecToStack 'cmd /c mklink /d ironscheme "$INSTDIR\ironscheme"'
  nsExec::ExecToStack 'cmd /c mklink /d srfi "$INSTDIR\srfi"'
  nsExec::ExecToStack 'cmd /c mklink /d lib "$INSTDIR\lib"'
  WriteUninstaller "$INSTDIR\uninstall.exe"
  WriteRegStr HKLM "${PRODUCT_DIR_REGKEY}" "" "$INSTDIR\IronScheme.Console.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayName" "$(^Name)"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "UninstallString" "$INSTDIR\uninstall.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayIcon" "$INSTDIR\IronScheme.Console.exe"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "DisplayVersion" "${PRODUCT_VERSION}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "URLInfoAbout" "${PRODUCT_WEB_SITE}"
  WriteRegStr ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}" "Publisher" "${PRODUCT_PUBLISHER}"
SectionEnd

Function un.IsDotNETInstalled
   Push $0
   Push $1
   Push $2
   Push $3
   Push $4

   ReadRegStr $4 HKEY_LOCAL_MACHINE "Software\Microsoft\.NETFramework" "InstallRoot"
   # remove trailing back slash
   Push $4
   Exch $EXEDIR
   Exch $EXEDIR
   Pop $4
   # if the root directory doesn't exist .NET is not installed
   ;DetailPrint "InstallRoot = $4"
   IfFileExists $4 0 noDotNET

   StrCpy $0 0
   StrCpy $NETROOT $4

 EnumStart:
   EnumRegKey $2 HKEY_LOCAL_MACHINE "Software\Microsoft\.NETFramework\Policy"  $0
   IntOp $0 $0 + 1
   ;DetailPrint "Policy = $2"
   StrCmp $2 "" noDotNET
   StrCpy $1 0

 EnumPolicy:
   EnumRegValue $3 HKEY_LOCAL_MACHINE "Software\Microsoft\.NETFramework\Policy\$2" $1
   IntOp $1 $1 + 1
   ;DetailPrint "PolicyKey = $3"
   StrCmp $3 "" EnumStart
!ifmacrondef NET4
   ;DetailPrint "Check for .NET 2 @ $4\v2.0.$3"
   IfFileExists "$4\v2.0.$3" foundDotNET2 
!else
   ;DetailPrint "Check for .NET 4 @ $4\v4.0.$3"
   IfFileExists "$4\v4.0.$3" foundDotNET4 
!endif
   Goto EnumPolicy

  noDotNET:
    DetailPrint ".NET not detected."
    StrCpy $0 0
    Goto done

!ifmacrondef NET4
  foundDotNET2:
    DetailPrint ".NET 2.0 detected @ $4\v2.0.$3."
    StrCpy $0 "$4\v2.0.$3"
    Goto done
!else
  foundDotNET4:
    DetailPrint ".NET 4.0 detected @ $4\v4.0.$3."
    StrCpy $0 "$4\v4.0.$3"
    Goto done
!endif

  done:
    Pop $4
    Pop $3
    Pop $2
    Pop $1
    Exch $0
FunctionEnd

Function un.onUninstSuccess
  HideWindow
  MessageBox MB_ICONINFORMATION|MB_OK "$(^Name) was successfully removed from your computer."
FunctionEnd

Function un.onInit
  MessageBox MB_ICONQUESTION|MB_YESNO|MB_DEFBUTTON2 "Are you sure you want to completely remove $(^Name) and all of its components?" IDYES +2
  Abort

  ${If} ${RunningX64}
    StrCpy $INSTDIR "$PROGRAMFILES64\IronScheme"
    SetRegView 64
  ${Else}
    StrCpy $INSTDIR "$PROGRAMFILES\IronScheme"
    SetRegView 32
  ${EndIf}
  
FunctionEnd

Section Uninstall
  Call un.IsDotNETInstalled
  Pop $NETPATH

  DetailPrint "Removing native images..."
  nsExec::ExecToStack '"$NETPATH\ngen.exe" uninstall "$INSTDIR\IronScheme.Console.exe"'
  nsExec::ExecToStack '"$NETPATH\ngen.exe" uninstall "$INSTDIR\ironscheme.boot.dll"'
  
  Delete "$DESKTOP\IronScheme.lnk"
	
  RMDir /r "$SMPROGRAMS\IronScheme"
  RMDir /r "$INSTDIR"

  DeleteRegKey ${PRODUCT_UNINST_ROOT_KEY} "${PRODUCT_UNINST_KEY}"
  DeleteRegKey HKLM "${PRODUCT_DIR_REGKEY}"
  SetAutoClose true
SectionEnd
