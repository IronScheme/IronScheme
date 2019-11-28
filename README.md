IronScheme
==========

IronScheme aims to be a R6RS conforming Scheme-like implementation based on the Microsoft DLR.

IronScheme implements over 99% of the R6RS specification and specified behavior.

[![Build status](https://ci.appveyor.com/api/projects/status/github/leppie/IronScheme?branch=master&svg=true)](https://ci.appveyor.com/project/leppie/ironscheme/branch/master)

Building
========

1. Open the `IronSchemeCore.sln` file in VS2019
2. Set startup project as `IronScheme.Console`

Alternatively, using the `build.cmd` batchfile in the IronScheme directory.

Running on Windows
==================

Run the `IronScheme.Console.exe` for the runtime and bitness you want to target. V2 requires .NET3.5. V4 requires requires .NET4.0 or higher.

Running on Linux/MacOS
======================
```
wget https://dot.net/v1/dotnet-install.sh
chmod +x dotnet-install.sh
dotnet-install.sh --channel Current --runtime dotnet # v2.1+
export -p PATH="$HOME/.dotnet:$PATH"

wget https://github.com/leppie/IronScheme/releases/download/<latest release>.zip
unzip <latest release>.zip
cd IronScheme
alias ironscheme="dotnet IronScheme.ConsoleCore.dll"
ironscheme
# if you prefer Mono
mono  IronScheme.Console-v4.exe
```

Running tests
=============

After building, you can use NUnit to run `IronScheme.Tests.dll` in the `IronScheme.Console` bin folder. 

There is also a batch file in the IronScheme directory called `test.cmd`. You can pass the `/verbose` to the batch file to see complete output in case of failures.

**Note**

Make sure that `peverify` is in your PATH. The test fixtures are designed to execute in a specfic order. The tests will not take less than 6 minutes to complete.

Nuget feed
==========

On Nuget.org and CI-builds on https://ci.appveyor.com/nuget/ironscheme

Issues
======

Please @leppie in your issues/PR's, else I will probably not know about it. I do not actively monitor Github. If I still dont answers, nag me on Twitter, also @leppie.

Other
=====

* http://ironscheme.codeplex.com/ (old site)
    
