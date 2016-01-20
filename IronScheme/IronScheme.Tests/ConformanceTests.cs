using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;

namespace IronScheme.Tests
{
  public class ConformanceTests : TestRunner
  {
    [Test]
    public void R6RS()
    {
      var r = RunTest(@"tests\r6rs\run.sps");
      Console.WriteLine(r.Output);
      Console.WriteLine("Expected 8 failed tests.");
      Assert.True(r.Output.Contains("8 of 8970 tests failed."));
    }

    [Test]
    public void Trig()
    {
      var r = RunTest(@"tests\trigtest.sps");
      Console.WriteLine(r.Output);
      Assert.True(r.Output.Contains("Passed all 17699 tests."));
    }

    [Test]
    public void CLispNumbers()
    {
      var r = RunTest(@"tests\clisp-number-tests.sps");
      
      Console.WriteLine(r.Output);
      Console.WriteLine("Expected 3 failed tests.");
      Assert.True(r.Output.Contains("Failed 3 of 2476 tests."));
    }

    [Test]
    public void FloatingPoint()
    {
      var r = RunTest(@"tests\fp-test.sps");
      Console.WriteLine(r.Output);
      Assert.True(r.Output.Length == 0);
    }
  }
}
