using System;
using System.Collections.Generic;
using System.Text;
using NUnit.Framework;
using System.Diagnostics;
using System.IO;

namespace IronScheme.Tests
{
  [TestFixture]
  public abstract class TestRunner
  {
    protected class TestResult
    {
      public string Output;
    }

    protected TestResult RunIronSchemeTestWithInput(string input)
    {
      return RunIronSchemeTest(null, input);
    }

    protected TestResult RunIronSchemeTest(string args)
    {
      return RunIronSchemeTest(args, null);
    }

    protected TestResult RunIronSchemeTest(string args, string input)
    {
      return RunTest("IronScheme.Console32.exe", args, input);
    }

    protected TestResult RunTest(string exe, string args)
    {
      return RunTest(exe, args, null);
    }

    protected TestResult RunTest(string exe, string args, string input)
    {
      var p = new Process
      {
        StartInfo = new ProcessStartInfo
        {
          FileName = exe,
          RedirectStandardOutput = true,
          StandardOutputEncoding = Encoding.UTF8,
          RedirectStandardError = true,
          RedirectStandardInput = true,
          UseShellExecute = false,
          CreateNoWindow = true,
          Arguments = args,
        },
      };

      try
      {
        if (!p.Start())
        {
          Assert.Fail("could not start {0}", exe);
        }

        if (input != null)
        {
          p.StandardInput.WriteLine(input);
          p.StandardInput.WriteLine("(exit)");
          p.StandardInput.Flush();
          p.StandardInput.Close();
        }

        var r = new TestResult
        {
          Output = p.StandardOutput.ReadToEnd()
        };

        var exited = p.WaitForExit(180000);

        if (!exited)
        {
          p.Kill();
        }
 
        Assert.AreEqual(0, p.ExitCode);

        return r;
      }
      catch (Exception ex)
      {
        if (!p.HasExited)
        {
          p.Kill();
        }
        Assert.Fail("runner failed: {0}", ex.Message);
        throw;
      }
    }
  }
}
