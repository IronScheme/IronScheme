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
    protected TestRunner()
    {
      Quiet = Environment.GetEnvironmentVariable("QUIET") != null;
      TestCore = Environment.GetEnvironmentVariable("TESTCORE") != null;
    }

    public bool TestCore { get; private set; }

    protected static void AssertError(TestResult tr)
    {
      Assert.IsEmpty(tr.Error);
    }

    protected bool Quiet { get; private set; }

    protected class TestResult
    {
      public string Output;
      public string Error;
    }

    protected TestResult RunIronSchemeTestWithInput(string input)
    {
      return RunIronSchemeTest(null, input);
    }

    protected TestResult RunIronSchemeTest(string args)
    {
      return RunIronSchemeTest(args, null);
    }

    protected TestResult RunIronSchemeTest(string args, bool echo)
    {
      return RunIronSchemeTest(args, null, echo);
    }

    protected TestResult RunIronSchemeTest(string args, string input)
    {
      return RunIronSchemeTest(args, input, true);
    }

    protected TestResult RunIronSchemeTest(string args, string input, bool echo)
    {
      if (TestCore)
      {
        return RunTest("C:\\Program Files\\dotnet\\dotnet.exe", "IronScheme.Console.dll " + args, input, echo);
      }
      return RunTest( "IronScheme.Console.exe", args, input, echo);
    }

    protected TestResult RunTest(string exe, string args, bool echo)
    {
      return RunTest(exe, args, null, echo);
    }

    protected TestResult RunTest(string exe, string args)
    {
      return RunTest(exe, args, null);
    }

    protected TestResult RunTest(string exe, string args, string input)
    {
      return RunTest(exe, args, input, true);
    }

    protected TestResult RunTest(string exe, string args, string input, bool echo)
    {
      
      var error = new StringWriter();
      var output = new StringWriter();

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
        }
      };

      p.ErrorDataReceived += (s, e) =>
      {
        if (echo && !Quiet) Console.Error.WriteLine(e.Data);
        error.WriteLine(e.Data);
      };

      p.OutputDataReceived += (s, e) =>
      {
        if (echo && !Quiet) Console.WriteLine(e.Data);
        output.WriteLine(e.Data);
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

        p.BeginErrorReadLine();
        p.BeginOutputReadLine();

        var exited = p.WaitForExit(300000);

        var r = new TestResult
        {
          Output = output.ToString().TrimEnd(Environment.NewLine.ToCharArray()),
          Error = error.ToString().TrimEnd(Environment.NewLine.ToCharArray())
        };

        if (!exited)
        {
          p.Kill();
        }

        if (p.ExitCode != 0 && !echo)
        {
          if (r.Output.Length > 0)
          {
            Console.WriteLine(r.Output);
          }
          if (r.Error.Length > 0)
          {
            Console.Error.WriteLine(r.Error);
          }
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
        Assert.Fail("runner failed: {0}", ex);
        throw;
      }
    }
  }
}
