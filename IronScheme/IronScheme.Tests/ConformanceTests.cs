using System;
using NUnit.Framework;

namespace IronScheme.Tests
{
  [Parallelizable(scope: ParallelScope.All | ParallelScope.Fixtures)]
  [Order(2)]
  [Category(nameof(Conformance))]
  public class Conformance : TestRunner
  {
    [Test]
    public void R6RS()
    {
      var r = RunIronSchemeTest(@"tests\r6rs\run.sps");
      if (!Quiet) Console.WriteLine("Expected 3 failed tests.");
      Assert.That(r.Output, Does.Contain("3 of 8971 tests failed."));
      AssertError(r);
    }

    [Test]
    public void Trig()
    {
      var r = RunIronSchemeTest(@"tests\trigtest.sps");
      if (!Quiet) Console.WriteLine("Expected 8 failed tests.");
      Assert.That(r.Output, Does.Contain("Failed 8 of 17707 tests."));
      AssertError(r);
    }

    [Test]
    public void CLispNumbers()
    {
      var r = RunIronSchemeTest(@"tests\clisp-number-tests.sps");
      if (!Quiet) Console.WriteLine("Expected 3 failed tests.");
      Assert.That(r.Output, Does.Contain("Failed 3 of 2476 tests."));
      AssertError(r);
    }

    [Test]
    public void FloatingPoint()
    {
      var r = RunIronSchemeTest(@"tests\fp-test.sps");
      Assert.That(r.Output, Is.Empty);
      AssertError(r);
    }
  }
}
