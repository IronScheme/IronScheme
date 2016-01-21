using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;

namespace IronScheme.Tests
{
  public class ReleaseTests : TestRunner
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

    }
  }

  public class DebugTests : TestRunner
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

    }
  }
}
