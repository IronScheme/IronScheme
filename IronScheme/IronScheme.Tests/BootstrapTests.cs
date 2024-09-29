using System;
using System.IO;
using NUnit.Framework;

namespace IronScheme.Tests
{
  [Order(1)]
  [Category(nameof(Bootstrap))]
  public class Bootstrap : TestRunner
  {
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

      Assert.Pass();
    }

    [Test]
    [Order(3)]
    public void Verify_Debug()
    {
      var libs = File.ReadAllLines("compiled.lst");

      try
      {
        foreach (var lib in libs)
        {
          VerifyAssembly(lib);
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

      Assert.Pass();
    }

    [Test]
    [Order(13)]
    public void Verify_Release()
    {
      var libs = File.ReadAllLines("compiled.lst");
      foreach (var lib in libs)
      {
        VerifyAssembly(lib);
      }
    }

    static string _sdkRefPath;

    string SdkRefPath => _sdkRefPath ??= GetSdkRefPath();  

    string GetSdkRefPath()
    { 
      var v = RunTest("dotnet", "--info");
      var lines = v.Output.Split(new[] { Environment.NewLine }, StringSplitOptions.None);
      var hi = Array.IndexOf(lines, "Host:");
      var sdkVer = lines[hi + 1].Replace("Version:", "").Trim();

      return $@"c:\Program Files\dotnet\shared\Microsoft.NETCore.App\{sdkVer}\*.dll";
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

        var r = RunTest("ilverify", $@"{assembly} -r ""{SdkRefPath}"" -r ""*.dll""");
        Assert.That(r.Output, Is.EqualTo($"All Classes and Methods in {Path.GetFullPath(assembly)} Verified.").IgnoreCase);
      }
    }
  }
}
