﻿using System;
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
      var libs = File.ReadAllLines("compiled.lst");      

      if (!Quiet) Console.WriteLine("Deleting system libraries");
      foreach (var lib in libs)
      {
        File.Delete(lib);
      }

      File.Delete("compiled.lst");

      if (!Quiet) Console.WriteLine("Done");
    }
  }

}
