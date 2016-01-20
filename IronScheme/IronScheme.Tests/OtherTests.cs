using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using System.Diagnostics;

namespace IronScheme.Tests
{
  public class OtherTests : TestRunner
  {
    //[Test]
    public void PFDS()
    {
      var r = RunTest(@"lib\pfds\tests.scm");
      Debugger.Break();
      Console.WriteLine(r.Output);
      Console.WriteLine("Expected 8 failed tests.");
      Assert.True(r.Output.Contains("8 of 8970 tests failed."));
    }

    //[Test]
    public void MiniKanren()
    {

    }
  }
}
