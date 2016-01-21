cinst ilmerge
cinst zip
curl -o xz.zip http://tukaani.org/xz/xz-5.2.1-windows.zip
unzip -o xz.zip -d xz bin_i686-sse2/xz.exe
copy xz\bin_i686-sse2\xz.exe "C:\Program Files\Git\usr\bin\"
set PATH=%PATH%;C:\Program Files (x86)\Microsoft SDKs\Windows\v10.0A\bin\NETFX 4.6.1 Tools\;