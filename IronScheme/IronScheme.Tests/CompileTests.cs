using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using System.IO;

namespace IronScheme.Tests.Compile
{
  public class Release : TestRunner
  {
    [Test]
    public void Compile()
    {
      RunIronSchemeTest(@"compile-system-libraries.sps");
      Directory.Move("lib", "lib.hide");
      RunIronSchemeTest(@"compile-system-libraries.sps");
      Directory.Move("lib.hide", "lib");
    }

    [Test]
    public void Verify()
    {
      var libs = File.ReadAllLines("compiled.lst");

      foreach (var lib in libs)
      {
        if (!Quiet) Console.WriteLine("Verifying: " + lib);
        var r = RunTest("peverify.exe", "/nologo " + lib, false);
        Assert.True(r.Output.Contains("All Classes and Methods in " + lib + " Verified."));
      }

      Assert.Pass("incredible!");
    }
  }

  public class Debug : TestRunner
  {
    [Test]
    public void Compile()
    {
      var r = RunIronSchemeTest(@"-debug compile-system-libraries.sps");
      var compiledlibs = r.Output;
      var list = Array.ConvertAll(compiledlibs.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries), l => l.Replace("compiling ", ""));

      File.WriteAllLines("compiled.lst", list);

      Directory.Move("lib", "lib.hide");
      RunIronSchemeTest(@"-debug compile-system-libraries.sps");
      Directory.Move("lib.hide", "lib");
    }

    [Test]
    public void Verify()
    {
      var libs = File.ReadAllLines("compiled.lst");

      try
      {
        foreach (var lib in libs)
        {
          if (!Quiet) Console.WriteLine("Verifying: " + lib);
          var r = RunTest("peverify.exe", "/nologo " + lib, false);
          Assert.True(r.Output.Contains("All Classes and Methods in " + lib + " Verified."));
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
