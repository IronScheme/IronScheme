using NUnit.Framework;

namespace IronScheme.Tests
{
  [Category(nameof(Other))]
  public class Other : TestRunner
  {
    [Test]
    //[Ignore("test is flakey")]
    public void PFDS()
    {
      var r = RunIronSchemeTest(@"lib\pfds\tests.scm");
      Assert.That(r.Output, Does.Contain("255 tests, 255 passed (100%), 0 failed (0%)"));
      AssertError(r);
    }

    [Test]
    public void MiniKanren()
    {
      var r = RunIronSchemeTestWithInput(@"(include ""lib/minikanren/mktests.scm"")");
      Assert.That(r.Output, Does.Contain("Ignoring divergent test 10.62"));
      AssertError(r);
    }
  }
}
