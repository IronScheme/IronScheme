#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Diagnostics;
using System.Text;
using IronScheme.Hosting;

namespace IronScheme.Runtime
{
  class Program
  {
    static int Main(string[] args)
    {
      if (args.Length > 0 && args[0] == "-profile")
      {
        const string PROFILER_GUID = "{9E2B38F2-7355-4C61-A54F-434B7AC266C0}";

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
        Array.Copy(args, 1, argargs, 0, argargs.Length);

        psi.Arguments = string.Join(" ", argargs);
        psi.UseShellExecute = false;
        
        Process p = Process.Start(psi);
        //p.PriorityBoostEnabled = true;

        p.WaitForExit();

        return p.ExitCode;
      }
      else
      {
        //Encoding oi = Console.InputEncoding;
        Encoding oo = Console.OutputEncoding;

        //Debugger.Launch();

        try
        {
          //Console.InputEncoding = Encoding.UTF8;
            Console.OutputEncoding = Encoding.UTF8;
          return new IronSchemeConsoleHost().Run(args);
        }
        finally
        {
          Console.OutputEncoding = oo;
          //Console.InputEncoding = oi;
        }
      }
    }
  }
}
