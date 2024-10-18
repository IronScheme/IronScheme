using System;
using NUnit.Framework;
using System.IO;

namespace IronScheme.Tests
{
  [Order(100)]
  [Category(nameof(Teardown))]
  public class Teardown : TestRunner
  {
    [Test]
    public void Run()
    {
      Setup.Cleanup();
    }
  }
}
