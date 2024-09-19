using System;
using NUnit.Framework;
using System.IO;

namespace IronScheme.Tests.Verify
{
  public class Release : TestRunner
  {
    [Test]
    public void Verify()
    {
      var libs = File.ReadAllLines("compiled.lst");

      if (!TestCore)
      {
        foreach (var lib in libs)
        {
          if (!Quiet) Console.WriteLine("Verifying: " + lib);
          var r = RunTest("peverify.exe", "/nologo " + lib, false);
          Assert.That(r.Output, Is.EqualTo("All Classes and Methods in " + lib + " Verified."));
        }
      }
      else
      {
        var v = RunTest("dotnet", "--info");
        var lines = v.Output.Split(new[] { Environment.NewLine }, StringSplitOptions.None);
        var hi = Array.IndexOf(lines, "Host:");
        var sdkVer = lines[hi + 1].Replace("Version:", "").Trim();

        var refPath = $@"c:\Program Files\dotnet\shared\Microsoft.NETCore.App\{sdkVer}\*.dll";

        foreach (var lib in libs)
        {
          if (!Quiet) Console.WriteLine("Verifying: " + lib);
          var r = RunTest("ilverify", $@"{lib} -r ""{refPath}"" -r ""*.dll""", false);
          Assert.That(r.Output, Is.EqualTo("All Classes and Methods in " + Path.GetFullPath(lib) + " Verified.").IgnoreCase);
        }
      }

      Assert.Pass("incredible!");
    }
  }

  public class Debug : TestRunner
  {
    [Test]
    public void Verify()
    {
      var libs = File.ReadAllLines("compiled.lst");

      try
      {
        if (!TestCore)
        {
          foreach (var lib in libs)
          {
            if (!Quiet) Console.WriteLine("Verifying: " + lib);
            var r = RunTest("peverify.exe", "/nologo " + lib, false);
            Assert.That(r.Output, Is.EqualTo("All Classes and Methods in " + lib + " Verified."));
          }
        }
        else
        {
          var v = RunTest("dotnet", "--info");
          var lines = v.Output.Split(new[] { Environment.NewLine }, StringSplitOptions.None);
          var hi = Array.IndexOf(lines, "Host:");
          var sdkVer = lines[hi + 1].Replace("Version:", "").Trim();

          var refPath = $@"c:\Program Files\dotnet\shared\Microsoft.NETCore.App\{sdkVer}\*.dll";

          foreach (var lib in libs)
          {
            if (!Quiet) Console.WriteLine("Verifying: " + lib);
            var r = RunTest("ilverify", $@"{lib} -r ""{refPath}"" -r ""*.dll""", false);
            Assert.That(r.Output, Is.EqualTo("All Classes and Methods in " + Path.GetFullPath(lib) + " Verified.").IgnoreCase);
          }
        }
      }
      finally
      {
        foreach (var lib in libs)
        {
          File.Delete(lib);
          File.Delete(Path.ChangeExtension(lib, "pdb"));
        }
      }
    }
  }
}
