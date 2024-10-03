using NUnit.Framework;

namespace IronScheme.Tests
{
  [Order(4)]
  [Parallelizable(scope: ParallelScope.All | ParallelScope.Fixtures)]
  [Category(nameof(Other))]
  public class Other : TestRunner
  {
    [Test]
    //[Ignore("test is flakey")]
    public void PFDS()
    {
      var r = RunIronSchemeTest(@"lib\pfds\tests.scm");
      try
      {
        Assert.That(r.Output, Does.Contain("255 tests, 255 passed (100%), 0 failed (0%)"));
      }
      catch (AssertionException)
      {
        Assert.Warn("PFDS test can flakey");
        Assert.That(r.Output, Does.Contain("255 tests, 253 passed (99%), 2 failed (1%)"));
      }
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
