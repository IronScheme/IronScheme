$packageName = 'IronScheme'
$url = 'http://build.ironscheme.net/IronScheme-TFSREV.zip' 

Install-ChocolateyZipPackage "$packageName" "$url" "$(Split-Path -parent $MyInvocation.MyCommand.Definition)" 
