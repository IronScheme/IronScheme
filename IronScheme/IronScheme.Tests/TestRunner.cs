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
      public int ExitCode;
    }

    protected TestResult RunTestWithInput(string input)
    {
      return RunTest(null, input);
    }

    protected TestResult RunTest(string filename)
    {
      return RunTest(filename, null);
    }

    protected TestResult RunTest(string filename, string input)
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
          Arguments = filename,
        },
        //EnableRaisingEvents = true,
      };

      //p.OutputDataReceived += (s, e) => Console.Write(e.Data);

      try
      {
        p.Start();

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

        var exited = p.WaitForExit(12000000);

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
