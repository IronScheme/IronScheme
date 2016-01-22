IronScheme
==========

Main repo for IronScheme. Codeplex/TFS will not be be maintained anymore.

[![Build status](https://ci.appveyor.com/api/projects/status/github/leppie/IronScheme?branch=master&svg=true)](https://ci.appveyor.com/project/leppie/ironscheme)

You can download the latest build artefacts from the above link.

**Note:** Previous SVN mirror has been hosed due to Codeplex SVN not working anymore. You can find the last SVN version at https://gitlab.com/leppie/IronScheme. The history is identical.

Building
========

1. Open the `IronScheme2008.sln` file in VS2008, VS2010, VS2012, VS2013 or VS2015
2. Allow the project to be converted, if needed
3. Set startup project as `IronScheme.Console`
4. Set the solution profile to `Release` (optional, but recommended)

Alternatively, the following can be done from the VS (any version 2008+) command prompt:
```
msbuild IronSchemeCore.sln /v:m /p:TargetFrameworkVersion=v2.0 /p:Configuration=Release
```
**Note** 

This solution will only build the essentials, basically no web projects. 
You can also adjust the framework to your liking. Tested with 2.0, 4.0, 4.5 on the Developer Command Prompt for VS2015.

Notes
=====

- The CLR 4 debugger is rather slow. Recommended to stick to CLR 2 .NET versions.

