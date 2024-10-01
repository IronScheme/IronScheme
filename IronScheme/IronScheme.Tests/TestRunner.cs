using System;
using System.Text;
using NUnit.Framework;
using System.Diagnostics;
using System.IO;
using System.Runtime.CompilerServices;
using System.Threading;

namespace IronScheme.Tests
{
  [TestFixture]
  public abstract class TestRunner
  {
    [OneTimeSetUp]
    public void Setup()
    {
      var iswd = Environment.GetEnvironmentVariable("ISWD");
      if (iswd != null)
      {
        Environment.CurrentDirectory = iswd;
      }
      Quiet = Environment.GetEnvironmentVariable("QUIET") == "1";
      TestCore = Environment.GetEnvironmentVariable("TESTCORE") == "1";
    }

    public bool TestCore { get; private set; }

    protected static void AssertError(TestResult tr)
    {
      Assert.That(tr.Error, Is.Empty, "stderr is not empty");
    }

    protected static string NormalizeLineBreaks(string input)
    {
      using var sr = new StringReader(input);
      using var sw = new StringWriter();

      string line = null;

      while ((line = sr.ReadLine()) != null)
      {
        sw.WriteLine(line.Trim('\r','\n'));
      }

      return sw.ToString();
    }
     

    protected bool Quiet { get; private set; }

    protected class TestResult
    {
      public string Output;
      public string Error;

      public override string ToString()
      {
        return $@"Output:
{Output}
Error:
{Error}";
      }
    }

    protected TestResult RunIronSchemeTestWithInput(string input, [CallerMemberName] string method = null)
    {
      return RunIronSchemeTest(null, input, method);
    }

    protected TestResult RunIronSchemeTest(string args, [CallerMemberName] string method = null)
    {
      return RunIronSchemeTest(args, null, method);
    }

    protected TestResult RunIronSchemeTest(string args, string input, [CallerMemberName] string method = null)
    {
      if (TestCore)
      {
        return RunTest("dotnet", "IronScheme.ConsoleCore.dll " + args, input, method);
      }
      return RunTest("IronScheme.Console32.exe", args, input, method);
    }

    protected TestResult RunTest(string exe, string args)
    {
      return RunTest(exe, args, null);
    }

    protected static TestResult RunTest(string exe, string args, string input, string method = null)
    {
      var error = new StringWriter();
      var output = new StringWriter();

      using var p = new Process
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
          Arguments = args
        }
      };

      p.ErrorDataReceived += (s, e) =>
      {
        //if (echo && !Quiet) Console.Error.WriteLine(e.Data);
        error.WriteLine(e.Data);
      };

      p.OutputDataReceived += (s, e) =>
      {
        //if (echo && !Quiet) Console.WriteLine(e.Data);
        output.WriteLine(e.Data);
      };

      try
      {
        if (!p.Start())
        {
          Assert.Fail($"could not start {exe}");
        }

        if (input != null)
        {
          p.StandardInput.WriteLine(input);
          p.StandardInput.WriteLine("(exit)");
          p.StandardInput.Flush();
          p.StandardInput.Close();
          p.StandardInput.Dispose();
        }

        p.BeginErrorReadLine();
        p.BeginOutputReadLine();

        var exited = p.WaitForExit(Timeout.Infinite); // https://github.com/dotnet/runtime/issues/108395

        //output.WriteLine(p.StandardOutput.ReadToEnd());
        //error.WriteLine(p.StandardError.ReadToEnd());

        Assert.That(exited, Is.True);
        Assert.That(p.HasExited, Is.True);

        error.Flush();
        output.Flush();

        var r = new TestResult
        {
          Output = output.ToString().TrimEnd(Environment.NewLine.ToCharArray()),
          Error = error.ToString().TrimEnd(Environment.NewLine.ToCharArray())
        };

        if (method != null)
        {
          //File.WriteAllText($"{method}.output", r.ToString());
        }

        if (!exited)
        {
          p.Kill();
        }

        //if (p.ExitCode != 0 && !echo)
        //{
        //  if (r.Output.Length > 0)
        //  {
        //    Console.WriteLine(r.Output);
        //  }
        //  if (r.Error.Length > 0)
        //  {
        //    Console.Error.WriteLine(r.Error);
        //  }
        //}

        Assert.That(p.ExitCode, Is.EqualTo(0), $"{r}");

        return r;
      }
      catch (Exception ex)
      {
        try
        {
          if (!p.HasExited)
          {
            p.Kill();
          }
        }
        catch { }
        Assert.Fail($"runner failed: {ex}");
        throw;
      }
    }
  }
}
