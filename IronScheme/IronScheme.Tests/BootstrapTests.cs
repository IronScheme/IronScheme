using NUnit.Framework;

namespace IronScheme.Tests.Bootstrap
{
  public class Release : TestRunner
  {
    [Test]
    public void Bootstrap()
    {
      RunIronSchemeTest(@"ironscheme-buildscript.sps");
      RunIronSchemeTest(@"ironscheme-buildscript.sps");

      if (!TestCore)
      {
        var r = RunTest("peverify.exe", "/nologo ironscheme.boot.dll");
        Assert.That(r.Output, Is.StringContaining("All Classes and Methods in ironscheme.boot.dll Verified."));
      }

      Assert.Pass();
    }
  }

  public class Debug : TestRunner
  {
    [Test]
    public void Bootstrap()
    {
      RunIronSchemeTest(@"-debug ironscheme-buildscript.sps");
      RunIronSchemeTest(@"-debug ironscheme-buildscript.sps");

      if (!TestCore)
      {
        var r = RunTest("peverify.exe", "/nologo ironscheme.boot.dll");
        Assert.That(r.Output, Is.StringContaining("All Classes and Methods in ironscheme.boot.dll Verified."));
      }

      Assert.Pass();
    }
  }
}
