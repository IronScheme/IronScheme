using NUnit.Framework;

namespace IronScheme.Tests
{
  public class Other : TestRunner
  {
    [Test]
    [Ignore("test is flakey")]
    public void PFDS()
    {
      var r = RunIronSchemeTest(@"lib\pfds\tests.scm");
      Assert.That(r.Output, Is.StringContaining("255 tests, 255 passed (100%), 0 failed (0%)"));
      AssertError(r);
    }

    [Test]
    public void MiniKanren()
    {
      var r = RunIronSchemeTestWithInput(@"(include ""lib/minikanren/mktests.scm"")");
      Assert.That(r.Output, Is.StringContaining("Ignoring divergent test 10.62"));
      AssertError(r);
    }
  }
}
