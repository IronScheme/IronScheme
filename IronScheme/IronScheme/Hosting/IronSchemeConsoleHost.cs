#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using IronScheme.Runtime;
using Microsoft.Scripting.Hosting;
using System.Reflection;

namespace IronScheme.Hosting
{
  public sealed class IronSchemeConsoleHost : ConsoleHost
  {
    internal static string VERSION = GetVersion();
    private static string runtime = null;

    static string GetVersion()
    {
      foreach (var a in typeof(Builtins).Assembly.GetCustomAttributes(typeof(AssemblyInformationalVersionAttribute), false))
      {
        var info = (AssemblyInformationalVersionAttribute)a;
        if (info.InformationalVersion == "1.0.0.0")
        {
          return "Latest";
        }
        return info.InformationalVersion;
      }
      return "Latest";
    }

    string logo;
    public IronSchemeConsoleHost()
    {
      logo = string.Format("IronScheme {0} github.com/IronScheme © 2007-2022 Llewellyn Pritchard ", VERSION);
    }

    public static int Execute(string[] args)
    {
      return new IronSchemeConsoleHost().Run(args);
    }
    
    protected override void Initialize()
    {
      base.Initialize();
      Options.LanguageProvider = ScriptEnvironment.GetEnvironment().GetLanguageProvider(typeof(IronSchemeLanguageProvider));
    }

    protected override void PrintLogo()
    {
      if (Options.DisplayLogo == true)
      {
        Console.Write(logo);
        PrintRuntimeVersion();
      }
      else if (Options.RunAction == ConsoleHostOptions.Action.RunConsole && !LanguageProvider.InputRedirected)
      {
        // errrkkk
        var tokens = logo.Split(new string[] { "github.com/IronScheme" }, StringSplitOptions.None);

        ConsoleColor old = Console.ForegroundColor;
        Console.ForegroundColor = ConsoleColor.Green;
        Console.Write(tokens[0]);
        Console.ForegroundColor = ConsoleColor.White;
        Console.Write("github.com/IronScheme");
        Console.ForegroundColor = ConsoleColor.DarkGray;
        Console.Write(tokens[1]);

        Console.ForegroundColor = old;

        PrintRuntimeVersion();
      }
    }

    private static void PrintRuntimeVersion()
    {
      runtime = GetRuntimeVersion();
      Console.WriteLine("(" + runtime + ")");
    }

    private static string GetRuntimeVersion()
    {
      var version = Environment.Version.ToString(2);

      var ass = typeof(object).Assembly;

      var isCore = ass.FullName.StartsWith("System.Private.CoreLib");

      if (isCore && Environment.Version.Major <= 4)
      {
        var va =
          ((AssemblyFileVersionAttribute)ass.GetCustomAttributes(typeof(AssemblyFileVersionAttribute), false)[0])
          ?.Version ?? string.Empty;
        var v = new Version(va);
        if (v.Major == 4 && v.Minor == 6) // core 2.x
        {
          var rta = typeof(System.IO.FileAttributes).Assembly;
          v = rta.GetName().Version;

          version = "Core " + v.ToString(3).Replace("4.", "");
        }
        else if (v.Minor == 7)
        {
          version = "Core 3.0";
        }
        else if (v.Major < 5) // seems to work for core 3.1
        {
          version = "Core " + version;
        }
      }
      else if (Environment.Version.Major == 4)
      {
        var va =
          ((AssemblyFileVersionAttribute)ass.GetCustomAttributes(typeof(AssemblyFileVersionAttribute), false)[0])
          ?.Version ?? string.Empty;
        var v = new Version(va);
        version = v.ToString(2);
      }

      if (Builtins.IsMono)
      {
        runtime = string.Format("Mono .NET {1} {0}", IntPtr.Size == 8 ? "64-bit" : "32-bit", version);

      }
      else
      {
        runtime = string.Format(".NET {1} {0}", IntPtr.Size == 8 ? "64-bit" : "32-bit", version);
      }

      return runtime;
    }

    internal static object GetRuntime()
    {
      if (runtime == null)
      {
        runtime = GetRuntimeVersion();
      }
      return runtime;
    }
  }
}
