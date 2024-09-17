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
          Assert.That(r.Output, Is.StringContaining("All Classes and Methods in " + lib + " Verified."));
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
            Assert.That(r.Output, Is.StringContaining("All Classes and Methods in " + lib + " Verified."));
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
