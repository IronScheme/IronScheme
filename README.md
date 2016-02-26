IronScheme
==========

IronScheme aims to be a R6RS conforming Scheme-like implementation based on the Microsoft DLR.

IronScheme implements over 99% of the R6RS specification and specified behavior.

**Notice:** Main repo for IronScheme. Codeplex/TFS will not be be maintained anymore.

[![Build status](https://ci.appveyor.com/api/projects/status/github/leppie/IronScheme?branch=master&svg=true)](https://ci.appveyor.com/project/leppie/ironscheme/branch/master)

You can download the latest build artefacts from the above link.

**Note:** Previous SVN mirror has been hosed due to Codeplex SVN not working anymore. You can find the last SVN version at https://gitlab.com/leppie/IronScheme. The history is identical.

Building
========

1. Open the `IronSchemeCore.sln` file in VS2008, VS2010, VS2012, VS2013 or VS2015
2. Allow the project to be converted, if needed
3. Set startup project as `IronScheme.Console`
4. Set the solution profile to `Release` (optional, but recommended)

Alternatively, the following can be done from the VS (any version 2008+) command prompt:
```
msbuild IronSchemeCore.sln /v:m /p:TargetFrameworkVersion=v2.0 /p:Configuration=Release
```
or using the `build.cmd` batchfile in the IronScheme directory.

**Note** 

This solution will only build the essentials, basically no web projects. 
You can also adjust the framework to your liking. Tested with 2.0, 4.0, 4.5 on the Developer Command Prompt for VS2015.

Running tests
=============

After building, you can use NUnit to run `IronScheme.Tests.dll` in the `IronScheme.Console` bin folder. 

There is also a batch file in the IronScheme directory called `test.cmd`. You can pass the `/verbose` to the batch file to see complete output in case of failures.

**Note**

Make sure that `peverify` is in your PATH. The test fixtures are designed to execute in a specfic order. The tests will not take less than 6 minutes to complete.

Other
=====

* http://ironscheme.codeplex.com/
* http://ironscheme.codeplex.com/documentation
* http://ironscheme.codeplex.com/discussions

IRC chat
========
Join [#IronScheme](http://webchat.freenode.net/?channels=IronScheme) on FreeNode.

Notes
=====

- The CLR 4 debugger is rather slow. Recommended to stick to CLR 2 .NET versions.

