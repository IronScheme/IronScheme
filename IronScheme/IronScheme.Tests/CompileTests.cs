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
      if (!Quiet) Console.WriteLine("Bootstrapping");
      RunIronSchemeTest(@"ironscheme-buildscript.sps", false);
      if (!Quiet) Console.WriteLine("Compiling system libraries");
      RunIronSchemeTest(@"compile-system-libraries.sps", false);
      if (!Quiet) Console.WriteLine("Done");
    }
  }

  public class Teardown : TestRunner
  {
    [Test]
    public void Run()
    {
      if (!Quiet) Console.WriteLine("Collecting system libraries");
      var r = RunIronSchemeTest(@"--show-loaded-libraries compile-system-libraries.sps", false);
      var loadedlibs = r.Output;

      if (!Quiet) Console.WriteLine("Deleting system libraries");
      foreach (var lib in loadedlibs.Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries))
      {
        File.Delete(lib);
      }

      if (!Quiet) Console.WriteLine("Done");
    }
  }

}
