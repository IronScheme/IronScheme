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
      if (!Quiet) Console.WriteLine("Expected 3 failed tests.");
      Assert.True(r.Output.Contains("3 of 8971 tests failed."));
      AssertError(r);
    }

    [Test]
    public void Trig()
    {
      var r = RunIronSchemeTest(@"tests\trigtest.sps");
      if (!Quiet) Console.WriteLine("Expected 8 failed tests.");
      Assert.True(r.Output.Contains("Failed 8 of 17707 tests."));
      AssertError(r);
    }

    [Test]
    public void CLispNumbers()
    {
      var r = RunIronSchemeTest(@"tests\clisp-number-tests.sps");
      if (!Quiet) Console.WriteLine("Expected 3 failed tests.");
      Assert.True(r.Output.Contains("Failed 3 of 2476 tests."));
      AssertError(r);
    }

    [Test]
    public void FloatingPoint()
    {
      var r = RunIronSchemeTest(@"tests\fp-test.sps");
      Assert.True(r.Output.Length == 0);
      AssertError(r);
    }
  }
}
