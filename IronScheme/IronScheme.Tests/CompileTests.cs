using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using System.IO;

namespace IronScheme.Tests
{
  public class Setup : TestRunner
  {
    [Test]
    public void Run()
    {
      Console.WriteLine("Bootstrapping");
      RunIronSchemeTest(@"ironscheme-buildscript.sps");
      Console.WriteLine("Compiling system libraries");
      RunIronSchemeTest(@"compile-system-libraries.sps");
      Console.WriteLine("Done");
      Assert.Pass();
    }
  }

  public class Teardown : TestRunner
  {
    [Test]
    public void Run()
    {
      Console.WriteLine("Collecting system libraries");
      var r = RunIronSchemeTest(@"--show-loaded-libraries compile-system-libraries.sps");
      var loadedlibs = r.Output;

      Console.WriteLine("Deleting system libraries");
      foreach (var lib in loadedlibs.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries))
      {
        File.Delete(lib);
      }

      Console.WriteLine("Done");
      Assert.Pass();
    }
  }

}
