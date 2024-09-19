using System;
using NUnit.Framework;
using System.IO;

namespace IronScheme.Tests.Compile
{
  public class Release : TestRunner
  {
    [Test]
    public void Compile()
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
  }

  public class Debug : TestRunner
  {
    [Test]
    public void Compile()
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
  }
}
