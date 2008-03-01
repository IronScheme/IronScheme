#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Public License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Public License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting;
using System.Diagnostics;
using System.IO;

namespace IronScheme.Hosting
{
  public sealed class IronSchemeConsoleHost : ConsoleHost
  {
    string logo;
    public IronSchemeConsoleHost()
    {
      logo = string.Format("IronScheme {0} http://www.codeplex.com/IronScheme Copyright © leppie - {1}",
            typeof(IronSchemeConsoleHost).Assembly.GetName().Version,
#if DEBUG
 "Debug build"
#else
            "Release build"
#endif
);

      Console.Title = logo;

      if (Debugger.IsAttached)
      {
        Console.Title += " - Debugger Attached";
      }
    }

    protected override int ExecuteFile(string file, string[] args)
    {
      return base.ExecuteFile(file, args);
    }

    //slowest script runner in the world... :(
    protected override int RunFile(IScriptEngine engine, SourceUnit sourceUnit)
    {
      string cwd = Environment.CurrentDirectory;
      Environment.CurrentDirectory = Runtime.Builtins.ApplicationDirectory;
      try
      {
        engine.Execute(string.Format("(load \"{0}\")", "init.scm"));
      }
      finally
      {
        Environment.CurrentDirectory = cwd;
      }
      engine.Execute(string.Format("(load \"{0}\")", sourceUnit.ToString().Replace('\\', '/')));
      return 0;
    }

    protected override void Initialize()
    {
      base.Initialize();
      Options.LanguageProvider = ScriptEnvironment.GetEnvironment().GetLanguageProvider(typeof(IronSchemeLanguageProvider));
    }

    protected override void PrintLogo()
    {
      if (Options.RunAction != ConsoleHostOptions.Action.RunFiles)
      {
        ConsoleColor old = Console.ForegroundColor;
        Console.ForegroundColor = ConsoleColor.Green;
        Console.WriteLine(logo.Replace("©", "(c)"));
        Console.ForegroundColor = old;
      }
    }

    protected override void UnhandledException(IScriptEngine engine, Exception e)
    {
      base.UnhandledException(engine, e);
      ConsoleColor old = Console.ForegroundColor;
      Console.ForegroundColor = ConsoleColor.Red;
      Console.WriteLine(e.ToString());
      Console.WriteLine(e.StackTrace);
      Console.ForegroundColor = old;
      Console.WriteLine("Press enter to exit.");
      Console.ReadLine();
    }
  }
}
