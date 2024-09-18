using System;
using System.IO;
using NUnit.Framework;

namespace IronScheme.Tests.Bootstrap
{
  public class Release : TestRunner
  {
    [Test]
    public void Bootstrap()
    {
      RunIronSchemeTest(@"ironscheme-buildscript.sps");
      RunIronSchemeTest(@"ironscheme-buildscript.sps");

      if (!TestCore)
      {
        var r = RunTest("peverify.exe", "/nologo ironscheme.boot.dll");
        Assert.That(r.Output, Is.EqualTo("All Classes and Methods in ironscheme.boot.dll Verified.").IgnoreCase);
      }
      else
      {
        var v = RunTest("dotnet", "--info");
        var lines = v.Output.Split(new[] { Environment.NewLine }, StringSplitOptions.None);
        var hi = Array.IndexOf(lines, "Host:");
        var sdkVer = lines[hi + 1].Replace("Version:", "").Trim();

        var refPath = $@"c:\Program Files\dotnet\shared\Microsoft.NETCore.App\{sdkVer}\*.dll";
        var lib = "ironscheme.boot.dll";
        var r = RunTest("ilverify", $@"{lib} -r ""{refPath}"" -r ""*.dll""", false);
        Assert.That(r.Output, Is.EqualTo("All Classes and Methods in " + Path.GetFullPath(lib) + " Verified.").IgnoreCase);
      }
    }
  }

  public class Debug : TestRunner
  {
    [Test]
    public void Bootstrap()
    {
      RunIronSchemeTest(@"-debug ironscheme-buildscript.sps");
      RunIronSchemeTest(@"-debug ironscheme-buildscript.sps");

      if (!TestCore)
      {
        var r = RunTest("peverify.exe", "/nologo ironscheme.boot.dll");
        Assert.That(r.Output, Is.EqualTo("All Classes and Methods in ironscheme.boot.dll Verified.").IgnoreCase);
      }
      else
      {
        var v = RunTest("dotnet", "--info");
        var lines = v.Output.Split(new[] { Environment.NewLine }, StringSplitOptions.None);
        var hi = Array.IndexOf(lines, "Host:");
        var sdkVer = lines[hi + 1].Replace("Version:", "").Trim();

        var refPath = $@"c:\Program Files\dotnet\shared\Microsoft.NETCore.App\{sdkVer}\*.dll";
        var lib = "ironscheme.boot.dll";
        var r = RunTest("ilverify", $@"{lib} -r ""{refPath}"" -r ""*.dll""", false);
        Assert.That(r.Output, Is.EqualTo("All Classes and Methods in " + Path.GetFullPath(lib) + " Verified.").IgnoreCase);
      }
    }
  }
}
