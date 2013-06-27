#region License
/* Copyright (c) 2007-2013 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using IronScheme.Hosting;
using IronScheme.Remoting.Server;
using System.IO;
using System.Text.RegularExpressions;

namespace IronScheme.Runtime
{
  class Program
  {
    static string[] PrefixArgs(string[] args, params string[] extra)
    {
      var newargs = new string[args.Length + extra.Length];
      Array.Copy(extra, newargs, extra.Length);
      Array.Copy(args, 0, newargs, extra.Length, args.Length);
      return newargs;
    }

    static int Main(string[] args)
    {
      var exename = Path.GetFileNameWithoutExtension(Environment.GetCommandLineArgs()[0]);

      switch (exename)
      {
        case "isc":
        case "isc32":
          args = PrefixArgs(args, "-nologo");
          break;
      }

      int index;

      if ((index = Array.IndexOf(args, "-profile")) >= 0)
      {
        const string PROFILER_GUID = "{9E2B38F2-7355-4C61-A54F-434B7AC266C0}";

        if (ExecuteRegsvr32(false, true))
        {
          ProcessStartInfo psi;

          // create a process executor
          psi = new ProcessStartInfo(typeof(Program).Assembly.Location);

          // ----- SET THE CLR ENVIRONMENT VARIABLES -------------------

          // set the COR_PROFILER env var. This indicates to the CLR which COM object to
          // instantiate for profiling.
          if (psi.EnvironmentVariables.ContainsKey("COR_PROFILER") == true)
            psi.EnvironmentVariables["COR_PROFILER"] = PROFILER_GUID;
          else
            psi.EnvironmentVariables.Add("COR_PROFILER", PROFILER_GUID);

          ////// set the Cor_Enable_Profiling env var. This indicates to the CLR whether or
          ////// not we are using the profiler at all.  1 = yes, 0 = no.
          if (psi.EnvironmentVariables.ContainsKey("COR_ENABLE_PROFILING") == true)
            psi.EnvironmentVariables["COR_ENABLE_PROFILING"] = "1";
          else
            psi.EnvironmentVariables.Add("COR_ENABLE_PROFILING", "1");

          // ----- RUN THE PROCESS -------------------
          string[] argargs = new string[args.Length - 1];
          args = Array.FindAll(args, x => x != "-profile");

          psi.Arguments = string.Join(" ", argargs);
          psi.UseShellExecute = false;

          try
          {
            Process p = Process.Start(psi);
            //p.PriorityBoostEnabled = true;

            p.WaitForExit();

            return p.ExitCode;
          }
          finally
          {
            ExecuteRegsvr32(false, false);
          }
        }
        else
        {
          // remove -profile
          string[] argargs = new string[args.Length - 1];
          args = Array.FindAll(args, x => x != "-profile");

          args = argargs;
        }
      }

      args = Args(args, "--remoting-server", x => Host.Start());
      args = Args(args, "--show-loaded-libraries", x => Builtins.ShowImports = true);

      //Encoding oi = Console.InputEncoding;
      Encoding oo = Console.OutputEncoding;

      //Debugger.Launch();

      EnableMulticoreJIT();

      try
      {
        //Console.InputEncoding = Encoding.UTF8;
        Console.OutputEncoding = Encoding.UTF8;
        return IronSchemeConsoleHost.Execute(args);
      }
      finally
      {
        Console.OutputEncoding = oo;
        //Console.InputEncoding = oi;
      }

    }

    static string[] Args(string[] args, string argname, Action<string[]> handler)
    {
      var i = Array.IndexOf(args, argname);
      if (i >= 0)
      {
        var newargs = new List<string>();
        int j = 0;

        for (; j < i; j++)
        {
          newargs.Add(args[j]);
        }
        
        j++;

        var argargs = new List<string>();

        while (j < args.Length && !args[j].StartsWith("-"))
        {
          argargs.Add(args[j]);
          j++;
        }

        while (j < args.Length)
        {
          newargs.Add(args[j]);
          j++;
        }

        handler(argargs.ToArray());

        args = newargs.ToArray();
      }
      return args;
    }

    static void EnableMulticoreJIT()
    {
      var type = typeof(object).Assembly.GetType("System.Runtime.ProfileOptimization", false);
      if (type != null)
      {
        var sprmi = type.GetMethod("SetProfileRoot");
        var spmi = type.GetMethod("StartProfile");
        if (sprmi != null && spmi != null)
        {
          Action<string> SetProfileRoot = Delegate.CreateDelegate(typeof(Action<string>), sprmi) as Action<string>;
          Action<string> SetProfile = Delegate.CreateDelegate(typeof(Action<string>), spmi) as Action<string>;

          var dir = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "IronScheme\\JITCache");

          Directory.CreateDirectory(dir);

          SetProfileRoot(dir);
          SetProfile("IronScheme");
        }
      }
    }

    // https://github.com/sawilde/opencover/blob/master/main/OpenCover.Framework/ProfilerRegistration.cs
    static bool ExecuteRegsvr32(bool userRegistration, bool register)
    {
      const string UserRegistrationString = "/n /i:user";

      var ppath = GetProfilerPath();

      if (!File.Exists(ppath))
      {
        if (register)
        {
          Console.WriteLine("WARNING: Profiler DLL ({0}) not available in application directory. Profiling will be disabled.", Path.GetFileName(ppath));
        }

        return false;
      }

      var startInfo = new ProcessStartInfo(Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.System), "regsvr32.exe"),
                               string.Format("/s {2} {0} \"{1}\"", userRegistration ? UserRegistrationString : String.Empty,
                               ppath, register ? string.Empty : "/u")) { CreateNoWindow = true };

      var process = Process.Start(startInfo);
      process.WaitForExit();
      return true;
    }

    static string GetProfilerPath()
    {
      var dir = Path.GetDirectoryName(typeof(Program).Assembly.Location);
      return Path.Combine(dir, string.Format("IronScheme.Profiler.x{0}.dll", IntPtr.Size == 8 ? "64" : "86"));
    }
  }
}
