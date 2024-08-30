using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using System.IO;

namespace IronScheme.Tests.Bootstrap
{
  public class Release : TestRunner
  {
    [Test]
    public void Bootstrap()
    {
      RunIronSchemeTest(@"ironscheme-buildscript.sps");
      RunIronSchemeTest(@"ironscheme-buildscript.sps");
      
      var r = RunTest("peverify.exe", "/nologo ironscheme.boot.dll");
      Assert.True(r.Output.Contains("All Classes and Methods in ironscheme.boot.dll Verified."));
    }
  }

  public class Debug : TestRunner
  {
    [Test]
    public void Bootstrap()
    {
      RunIronSchemeTest(@"-debug ironscheme-buildscript.sps");
      RunIronSchemeTest(@"-debug ironscheme-buildscript.sps");

      var r = RunTest("peverify.exe", "/nologo ironscheme.boot.dll");
      Assert.True(r.Output.Contains("All Classes and Methods in ironscheme.boot.dll Verified."));
    }
  }
}
