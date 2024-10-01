using System;
using System.Text;
using Xunit;
using System.Diagnostics;
using System.IO;
using System.Runtime.CompilerServices;
using System.Threading;

namespace IronScheme.Tests
{
  public abstract class TestRunnerxUnit
  {
    protected TestRunnerxUnit()
    {
      Quiet = Environment.GetEnvironmentVariable("QUIET") == "1";
      TestCore = Environment.GetEnvironmentVariable("TESTCORE") == "1";
    }

    public bool TestCore { get; private set; }

    protected static void AssertError(TestResult tr)
    {
      Assert.Equal("", tr.Error);
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
        return RunTest("C:\\Program Files\\dotnet\\dotnet.exe", "IronScheme.ConsoleCore.dll " + args, input, method);
      }
      return RunTest( "IronScheme.Console32.exe", args, input, method);
    }

    protected TestResult RunTest(string exe, string args)
    {
      return RunTest(exe, args, null);
    }

    protected static TestResult RunTest(string exe, string args, string input, string method = null)
    {
      var error = new StringWriter();
      var output = new StringWriter();

      var iswd = Environment.GetEnvironmentVariable("ISWD") ?? ".";

      //Assert.That(File.Exists(Path.Combine(iswd, exe)), Is.True);

      //Console.WriteLine(Path.Combine(iswd, exe));

      using var p = new Process
      {
        StartInfo = new ProcessStartInfo
        {
          FileName = Path.Combine(iswd, exe),
          RedirectStandardOutput = true,
          StandardOutputEncoding = Encoding.UTF8,
          RedirectStandardError = true,
          RedirectStandardInput = input != null,
          UseShellExecute = false,
          CreateNoWindow = true,
          Arguments = args,
          WorkingDirectory = iswd,
        }
      };

      var evt = new ManualResetEvent(false);

      p.EnableRaisingEvents = true;

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

      p.Exited += (s, e) =>
      {
        evt.Set();
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

        evt.WaitOne();

        var exited = p.WaitForExit(300000);

        //output.WriteLine(p.StandardOutput.ReadToEnd());
        //error.WriteLine(p.StandardError.ReadToEnd());

        Assert.True(exited);
        Assert.True(p.HasExited);

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

        //if (!exited)
        //{
        //  p.Kill();
        //}

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

        Assert.Equal(0, p.ExitCode);

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
