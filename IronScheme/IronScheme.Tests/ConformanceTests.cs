using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;

namespace IronScheme.Tests
{
  public class Conformance : TestRunner
  {
    [Test]
    public void R6RS()
    {
      var r = RunIronSchemeTest(@"tests\r6rs\run.sps");
      Console.WriteLine("Expected 8 failed tests.");
      Assert.True(r.Output.Contains("8 of 8970 tests failed."));
    }

    [Test]
    public void Trig()
    {
      var r = RunIronSchemeTest(@"tests\trigtest.sps");
      Assert.True(r.Output.Contains("Passed all 17699 tests."));
    }

    [Test]
    public void CLispNumbers()
    {
      var r = RunIronSchemeTest(@"tests\clisp-number-tests.sps");
      Console.WriteLine("Expected 3 failed tests.");
      Assert.True(r.Output.Contains("Failed 3 of 2476 tests."));
    }

    [Test]
    public void FloatingPoint()
    {
      var r = RunIronSchemeTest(@"tests\fp-test.sps");
      Assert.True(r.Output.Length == 0);
    }
  }
}
