using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using System.IO;

namespace IronScheme.Tests
{
  public class Release : TestRunner
  {
    [Test]
    public void Bootstrap()
    {
      var r = RunIronSchemeTest(@"ironscheme-buildscript.sps");
      Console.WriteLine(r.Output);

      r = RunIronSchemeTest(@"ironscheme-buildscript.sps");
      Console.WriteLine(r.Output);
      
      r = RunTest("peverify.exe", "/nologo ironscheme.boot.dll");
      Console.WriteLine(r.Output);

      Assert.True(r.Output.Contains("All Classes and Methods in ironscheme.boot.dll Verified."));
    }

    [Test]
    public void Compile()
    {
      var r = RunIronSchemeTest(@"compile-system-libraries.sps");
      Console.WriteLine(r.Output);

      Directory.Move("lib", "lib.hide");

      r = RunIronSchemeTest(@"--show-loaded-libraries compile-system-libraries.sps");
      var loadedlibs = r.Output;

      Directory.Move("lib.hide", "lib");

      foreach (var lib in loadedlibs.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries))
      {
        File.Delete(lib);
      }
    }
  }

  public class Debug : TestRunner
  {
    [Test]
    public void Bootstrap()
    {
      var r = RunIronSchemeTest(@"-debug ironscheme-buildscript.sps");
      Console.WriteLine(r.Output);

      r = RunIronSchemeTest(@"-debug ironscheme-buildscript.sps");
      Console.WriteLine(r.Output);

      r = RunTest("peverify.exe", "/nologo /ignore=0x80131820 ironscheme.boot.dll");
      Console.WriteLine(r.Output);

      Assert.True(r.Output.Contains("All Classes and Methods in ironscheme.boot.dll Verified."));
    }

    [Test]
    public void Compile()
    {
      var r = RunIronSchemeTest(@"-debug compile-system-libraries.sps");
      Console.WriteLine(r.Output);

      Directory.Move("lib", "lib.hide");

      r = RunIronSchemeTest(@"-debug --show-loaded-libraries compile-system-libraries.sps");
      var loadedlibs = r.Output;

      Directory.Move("lib.hide", "lib");

      foreach (var lib in loadedlibs.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries))
      {
        File.Delete(lib);
        File.Delete(Path.ChangeExtension(lib, "pdb"));
      }
    }
  }
}
