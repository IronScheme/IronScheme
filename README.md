[![Build status](https://ci.appveyor.com/api/projects/status/github/IronScheme/IronScheme?branch=master&svg=true)](https://ci.appveyor.com/project/leppie/ironscheme/branch/master)
[![NuGet version](https://badge.fury.io/nu/IronScheme.Core.svg)](https://badge.fury.io/nu/IronScheme.Core)

[![Windows](https://github.com/IronScheme/IronScheme/actions/workflows/windows.yml/badge.svg?branch=master)](https://github.com/IronScheme/IronScheme/actions/workflows/windows.yml)
[![OSX](https://github.com/IronScheme/IronScheme/actions/workflows/osx.yml/badge.svg?branch=master)](https://github.com/IronScheme/IronScheme/actions/workflows/osx.yml)
[![Linux](https://github.com/IronScheme/IronScheme/actions/workflows/linux.yml/badge.svg?branch=master)](https://github.com/IronScheme/IronScheme/actions/workflows/linux.yml)



IronScheme
==========

IronScheme aims to be a R6RS conforming Scheme-like implementation for all .NET implementations and platforms.

IronScheme implements over 99% of the R6RS specification and specified behavior. 

IronScheme's macro system is based on psyntax, and thus behaves similar to other implementations using psyntax, ie Icarus, Vicare, Chez.

Building
========

1. Open the `IronSchemeCore.sln` file in VS2019/2022
2. Set startup project as `IronScheme.Console`

Alternatively, using the `build.cmd` batchfile in the IronScheme directory.

Note: the VS solution is for an already bootstrapped configuration. It can behave differently to the `IronScheme.dll` in the release package. This is mostly to just provide debugging and advanced usage of IronScheme.

Running on Windows
==================

Run the `IronScheme.Console.exe` for the runtime and bitness you want to target. 

V2 requires .NET3.5. V4 requires requires .NET4.0 or higher. Pretty much any running Windows should have one or both of those.

You can also run it with .NET Core, which has no persisted compilation functionality. It may load precompiled libraries, but if it fails to load, it will fallback to runtime compilation. A warning will be display in that case.

By default, it will run on the lowest supported .NET Core runtime installed (.NET Core 2.1 or higher).

To Run it on a specific .NET Core version, use the `--fx-version` option with `dotnet` or `IronScheme.ConsoleCore.exe`.

Example to run on .NET 6.0 when you have multiple framework versions installed:

```
dotnet --fx-version 6.0.0 IronScheme.ConsoleCore.dll <args...>
```
or
```
IronScheme.ConsoleCore.exe --fx-version 6.0.0 <args...>
```

To run on the latest installed .NET Core version:

```
dotnet --roll-forward LatestMajor IronScheme.ConsoleCore.dll <args...>
```
or
```
IronScheme.ConsoleCore.exe --roll-forward LatestMajor <args...>
```

Running on Linux/MacOS
=======================

The preferred way to run on non-Windows OS is to use .NET Core. You can also run IronScheme on Mono, but your mileage may vary.

```
# get .NET Core if you dont have it already
wget https://dot.net/v1/dotnet-install.sh
chmod +x dotnet-install.sh
dotnet-install.sh --channel Current --runtime dotnet # v2.1+
export -p PATH="$HOME/.dotnet:$PATH"
```

```
# Download 
wget https://github.com/IronScheme/IronScheme/releases/download/<latest release>.zip
unzip <latest release>.zip
```

```
cd IronScheme
alias ironscheme="dotnet IronScheme.ConsoleCore.dll"
# if you prefer Mono
# alias ironscheme="mono IronScheme.Console-v4.exe"

ironscheme
```

Library usage
=============

Reference `IronScheme.dll` from your project. 

```
using IronScheme;

class Program
{
    static void Main(string[] args)
    {
        "(display 'hello-world)".Eval();
    }
} 
```

Or with C# 9 and later as:

```
using IronScheme;

"(display 'hello-world)".Eval();
```

Compiling libraries
===================

When running on .NET Framework or .NET 9.0 or higher, you can precompile scheme libraries.

You can do to this by running `(compile-system-libraries)`, or `(compile)` after importing your libraries.

Running tests
=============

To run the R6RS test suite, call the `(ironscheme-test)` procedure. There are a few known failure cases (currently 3 for .NET desktop). This can be run from the release package as well to confirm IronScheme runs correctly on a new platform or framework.

After building, you can use NUnit to run `IronScheme.Tests.dll` in the `IronScheme.Console` bin folder.

There is also a batch file in the IronScheme directory called `test.cmd`. You can pass the `/verbose` to the batch file to see complete output in case of failures.

**Note**

Make sure that `peverify` is in your PATH. The test fixtures are designed to execute in a specfic order. The tests will not take less than 6 minutes to complete.

Nuget feed
==========

On Nuget.org and CI-builds on https://ci.appveyor.com/nuget/ironscheme

Issues/PR
=========

Please @leppie in your issues/PR's, else I will probably not know about it. I do not actively monitor Github. If I still dont answer, nag me on Twitter, also @leppie.
    
