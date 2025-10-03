using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using NUnit.Framework;

namespace IronScheme.Tests
{
  [Order(1)]
  [Category(nameof(Bootstrap))]
  public class Bootstrap : TestRunner
  {
    const int LIBSCOUNT = 230;

    [Test]
    [Order(1)]
    public void Bootstrap_Debug()
    {
      RunIronSchemeTest(@"-debug ironscheme-buildscript.sps");
      RunIronSchemeTest(@"-debug ironscheme-buildscript.sps");

      VerifyAssembly("ironscheme.boot.dll");
    }

    [Test]
    [Order(2)]
    public void Compile_Debug()
    {
      var r = RunIronSchemeTest(@"-debug compile-system-libraries.sps");
      var compiledlibs = r.Output;
      var list = Array.ConvertAll(compiledlibs.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries), l => l.Replace("compiling ", ""));

      File.Delete("compiled.lst");
      File.WriteAllLines("compiled.lst", list);

      Directory.Move("lib", "lib.hide");
      RunIronSchemeTest(@"-debug compile-system-libraries.sps");
      Directory.Move("lib.hide", "lib");

      Assert.That(list, Has.Length.EqualTo(LIBSCOUNT));
    }

    [Test]
    [Order(3)]
    public void Verify_Debug()
    {
      var libs = File.ReadAllLines("compiled.lst");

      try
      {
        VerifyAssemblies(libs);

        Assert.That(libs, Has.Length.EqualTo(LIBSCOUNT));
      }
      finally
      {
        Setup.Cleanup();
      }
    }

    [Test]
    [Order(11)]
    public void Bootstrap_Release()
    {
      RunIronSchemeTest(@"ironscheme-buildscript.sps");
      RunIronSchemeTest(@"ironscheme-buildscript.sps");

      VerifyAssembly("ironscheme.boot.dll");
    }

    [Test]
    [Order(12)]
    public void Compile_Release()
    {
      var r = RunIronSchemeTest(@"compile-system-libraries.sps");
      var compiledlibs = r.Output;
      var list = Array.ConvertAll(compiledlibs.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries), l => l.Replace("compiling ", ""));

      File.Delete("compiled.lst");
      File.WriteAllLines("compiled.lst", list);

      Directory.Move("lib", "lib.hide");
      RunIronSchemeTest(@"compile-system-libraries.sps");
      Directory.Move("lib.hide", "lib");

      Assert.That(list, Has.Length.EqualTo(LIBSCOUNT));
    }

    [Test]
    [Order(13)]
    public void Verify_Release()
    {
      var libs = File.ReadAllLines("compiled.lst");

      VerifyAssemblies(libs);

      Assert.That(libs, Has.Length.EqualTo(LIBSCOUNT));
    }

    static string _sdkRefPath;

    string SdkRefPath => _sdkRefPath ??= GetSdkRefPath();  

    string GetSdkRefPath()
    {
      var runtimes = RunTest("dotnet", "--list-runtimes").Output;

      var pathre = new Regex(@"^((?<tfm>.+)\s)?(?<ver>.+)\s\[(?<path>.+)\]$");

      foreach (var line in runtimes.Split(new[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries).Reverse())
      {
        var m = pathre.Match(line);
        if (m.Success)
        {
          var tfm = m.Groups["tfm"].Value;
          var ver = m.Groups["ver"].Value;
          var path = m.Groups["path"].Value;

          if (tfm == "Microsoft.NETCore.App")
          {
            var verpath = Path.Combine(path, ver, "*.dll");
            return verpath;
          }
        }
      }

      throw new Exception("Runtime path not found");
    }

    private void VerifyAssemblies(string[] assemblies)
    {
      if (TestCore)
      {
        var r = RunTest("ilverify", $@"-r ""{SdkRefPath}"" -r ""*.dll"" {string.Join(" ", assemblies)}");
        foreach (var assembly in assemblies)
        {
          Assert.That(r.Output, Does.Contain($"All Classes and Methods in {Path.GetFullPath(assembly)} Verified.").IgnoreCase);
        }
      }
      else
      {
        foreach (var assembly in assemblies)
        {
          VerifyAssembly(assembly);
        }
      }
    }

    private void VerifyAssembly(string assembly)
    {
      if (!TestCore)
      {
        var r = RunTest("peverify.exe", $"/nologo {assembly}");
        Assert.That(r.Output, Is.EqualTo($"All Classes and Methods in {assembly} Verified.").IgnoreCase);
      }
      else
      {
        var r = RunTest("ilverify", $@"-r ""{SdkRefPath}"" -r ""*.dll"" {assembly}");
        Assert.That(r.Output, Is.EqualTo($"All Classes and Methods in {Path.GetFullPath(assembly)} Verified.").IgnoreCase);
      }
    }
  }
}
