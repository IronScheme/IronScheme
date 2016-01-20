using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using System.Diagnostics;

namespace IronScheme.Tests
{
  [TestFixture]
  public abstract class TestRunner
  {
    protected class TestResult
    {
      public string Output;
      public int ExitCode;
    }

    protected TestResult RunTest(string filename)
    {
      var p = new Process
      {
        StartInfo = new ProcessStartInfo
        {
          FileName = "IronScheme.Console32.exe",
          RedirectStandardOutput = true,
          RedirectStandardError = true,
          RedirectStandardInput = true,
          UseShellExecute = false,
          CreateNoWindow = true,
          Arguments = filename
        }
      };

      p.Start();

      var exited = p.WaitForExit(30000);

      if (!exited)
      {
        p.Kill();
      }

      var r = new TestResult
      {
        ExitCode = p.ExitCode,
        Output = p.StandardOutput.ReadToEnd()
      };

      Assert.AreEqual(0, p.ExitCode);

      return r;
    }
  }
}
