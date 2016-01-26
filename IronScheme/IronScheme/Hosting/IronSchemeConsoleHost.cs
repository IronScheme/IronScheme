#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.IO;
using IronScheme.Runtime;
using Microsoft.Scripting;
using Microsoft.Scripting.Hosting;
using System.Reflection;

namespace IronScheme.Hosting
{
  public sealed class IronSchemeConsoleHost : ConsoleHost
  {
    internal static string VERSION = GetVersion();

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
      logo = string.Format("IronScheme {0} github.com/leppie/IronScheme © 2007,2008,2009,2010,2011,2012,2013,2014,2015,2016 Llewellyn Pritchard {1}",
          VERSION,
#if DEBUG
            "- Debug build "
#else
            ""
#endif
);
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

    static Stream ResourceLoader()
    {
      return typeof(IronSchemeLanguageProvider).Assembly.GetManifestResourceStream("ironscheme.libs");
    }

    protected override void PrintLogo()
    {
      if (Options.RunAction != ConsoleHostOptions.Action.RunFiles && !LanguageProvider.InputRedirected)
      {
        // errrkkk
        var tokens = logo.Split(new string[] { "github.com/leppie/IronScheme" }, StringSplitOptions.None);

        ConsoleColor old = Console.ForegroundColor;
        Console.ForegroundColor = ConsoleColor.Green;
        Console.Write(tokens[0]);
        Console.ForegroundColor = ConsoleColor.White;
        Console.Write("github.com/leppie/IronScheme");
        Console.ForegroundColor = ConsoleColor.DarkGray;

        tokens = tokens[1].Split('-');

        Console.Write(tokens[0]);
        if (tokens.Length > 1)
        {
          Console.ForegroundColor = ConsoleColor.Cyan;
          Console.Write(tokens[1].Substring(1)); // errrrkerr
        }
        Console.ForegroundColor = old;

        Console.WriteLine("(.NET {1} {0})", IntPtr.Size == 8 ? "64-bit" : "32-bit", Environment.Version.ToString(2));
        
      }
    }
  }
}
