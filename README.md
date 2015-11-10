IronScheme
==========

Mirror of Codeplex/TFS

**Note:** Previous SVN mirror has been hosed due to Codeplex SVN not working anymore. You can find the last SVN version at https://gitlab.com/leppie/IronScheme. The history is identical.

Building
========

1. Open the `IronScheme2008.sln` file in VS2008, VS2010, VS2012, VS2013 or VS2015
2. Allow the project to be converted, if needed
3. Set startup project as `IronScheme.Console`
4. Set the solution profile to `Release` (optional, but recommended)

Notes
=====

- The CLR 4 debugger is rather slow. Recommended to stick to CLR 2 .NET versions.

Git notes
=========

Set `autocrlf` to `false`, else TFS and Git does not play along when commiting.
