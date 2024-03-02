using System;
using NUnit.Framework;

namespace IronScheme.Tests
{
  public class Other : TestRunner
  {
    [Test]
    public void PFDS()
    {
      var r = RunIronSchemeTest(@"lib\pfds\tests.scm");
      Assert.True(r.Output.Contains("255 tests, 255 passed (100%), 0 failed (0%)"));
      AssertError(r);
    }

    [Test]
    public void MiniKanren()
    {
      var r = RunIronSchemeTestWithInput(@"(include ""lib/minikanren/mktests.scm"")");
      if (!Quiet)
      {
        Console.WriteLine("Output: " + r.Output);
      }
      
      Assert.True(r.Output.Contains("Ignoring divergent test 10.62"));
      
      AssertError(r);
    }
  }
}
