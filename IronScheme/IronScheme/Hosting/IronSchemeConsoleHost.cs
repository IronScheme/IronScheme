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
using IronScheme.Runtime;
using System.Collections;
using System.Text.RegularExpressions;
using System.Threading;

namespace IronScheme.Hosting
{
  public sealed class IronSchemeConsoleHost : ConsoleHost
  {
    const string VERSION = "1.0 beta 5";
    string logo;
    public IronSchemeConsoleHost()
    {
      logo = string.Format("IronScheme {0} ironscheme.codeplex.com © leppie 2007,2008,2009 {1}",
          VERSION,
            //typeof(IronSchemeConsoleHost).Assembly.GetName().Version,
#if DEBUG
#if CPS
            "Debug - CPS build"
#else
            "- Debug build "
#endif
#else
            ""
#endif
);

      //if (Debugger.IsAttached)
      //{
      //  Console.Title = string.Format("IronScheme {0} - Debugger Attached ", VERSION);
      //}
      //else
      //{
      //  Console.Title = string.Format("IronScheme {0} ", VERSION);
      //}
    }


    protected override void Initialize()
    {
      base.Initialize();
      Options.LanguageProvider = ScriptEnvironment.GetEnvironment().GetLanguageProvider(typeof(IronSchemeLanguageProvider));
    }

    protected override void PrintLogo()
    {
      if (Options.RunAction != ConsoleHostOptions.Action.RunFiles && !LanguageProvider.InputRedirected)
      {
        // errrkkk
        var tokens = logo.Split(new string[] { "ironscheme.codeplex.com" }, StringSplitOptions.None);

        ConsoleColor old = Console.ForegroundColor;
        Console.ForegroundColor = ConsoleColor.Green;
        Console.Write(tokens[0]);
        Console.ForegroundColor = ConsoleColor.White;
        Console.Write("ironscheme.codeplex.com");
        Console.ForegroundColor = ConsoleColor.DarkGray;

        tokens = tokens[1].Split('-');

        Console.Write(tokens[0]);
        if (tokens.Length > 1)
        {
          Console.ForegroundColor = ConsoleColor.Cyan;
          Console.Write(tokens[1].Substring(1)); // errrrkerr
        }
        Console.ForegroundColor = old;

        Console.WriteLine();
        
      }
    }

    protected override void UnhandledException(IScriptEngine engine, Exception e)
    {
      base.UnhandledException(engine, e);
    }
  }
}
