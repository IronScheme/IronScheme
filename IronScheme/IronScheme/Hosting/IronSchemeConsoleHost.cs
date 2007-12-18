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

namespace IronScheme.Hosting
{
  public class IronSchemeConsoleHost : ConsoleHost
  {
    string logo;
    public IronSchemeConsoleHost()
    {
#if !CRAZY
      ScriptDomainManager.Options.DebugMode = false;
      ScriptDomainManager.Options.EngineDebug = false;
      ScriptDomainManager.Options.DebugCodeGeneration = false;
      ScriptDomainManager.Options.OptimizeEnvironments = true;

#endif
      ScriptDomainManager.Options.AssemblyGenAttributes = 
#if DEBUG
      
      //Microsoft.Scripting.Generation.AssemblyGenAttributes.ILDebug |
      //Microsoft.Scripting.Generation.AssemblyGenAttributes.EmitDebugInfo |
      //Microsoft.Scripting.Generation.AssemblyGenAttributes.GenerateDebugAssemblies |
      //Microsoft.Scripting.Generation.AssemblyGenAttributes.DisableOptimizations |
      //Microsoft.Scripting.Generation.AssemblyGenAttributes.GenerateStaticMethods |
#endif
       Microsoft.Scripting.Generation.AssemblyGenAttributes.SaveAndReloadAssemblies;
      
      logo = string.Format("IronScheme {0} http://www.codeplex.com/IronScheme Copyright © leppie 2007 - {1} - {2}", 
            typeof(IronSchemeConsoleHost).Assembly.GetName().Version,
#if R6RS
            "R6RS mode"
#else
            "R5RS mode"
#endif
            ,
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


      ScriptDomainManager.Options.DynamicStackTraceSupport = false;
    }

    protected override void Initialize()
    {
      base.Initialize();
      Options.LanguageProvider = ScriptEnvironment.GetEnvironment().GetLanguageProvider(typeof(IronSchemeLanguageProvider));
    }

    protected override void PrintLogo()
    {
      ConsoleColor old = Console.ForegroundColor;
      Console.ForegroundColor = ConsoleColor.Green;
      Console.WriteLine(logo.Replace("©", "(c)"));
      Console.ForegroundColor = old;
    }

    protected override void UnhandledException(IScriptEngine engine, Exception e)
    {
      base.UnhandledException(engine, e);
      ConsoleColor old = Console.ForegroundColor;
      Console.ForegroundColor = ConsoleColor.Red;
      Console.WriteLine(e.ToString());
      Console.WriteLine(e.StackTrace);
      Console.ForegroundColor = old;
      Console.ReadLine();
    }
  }
}
