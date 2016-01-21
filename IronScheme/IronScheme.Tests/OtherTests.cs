using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using System.Diagnostics;

namespace IronScheme.Tests
{
  public class OtherTests : TestRunner
  {
    [Test]
    public void PFDS()
    {
      var r = RunIronSchemeTest(@"lib\pfds\tests.scm");
      Console.WriteLine(r.Output);
      Assert.True(r.Output.Contains("255 tests, 255 passed (100%), 0 failed (0%)"));
    }

    [Test]
    public void MiniKanren()
    {
      var r = RunIronSchemeTestWithInput(@"(include ""lib/minikanren/mktests.scm"")");
      Console.WriteLine(r.Output);
      Assert.True(r.Output.Contains("Ignoring divergent test 10.62"));
    }
  }
}
