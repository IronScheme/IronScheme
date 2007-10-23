#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Permissive License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Permissive License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting;

namespace IronScheme.Hosting
{
  public class IronSchemeConsoleHost : ConsoleHost
  {
    public IronSchemeConsoleHost()
    {
      ScriptDomainManager.Options.AssemblyGenAttributes |=
#if DEBUG
        Microsoft.Scripting.Generation.AssemblyGenAttributes.EmitDebugInfo |
        Microsoft.Scripting.Generation.AssemblyGenAttributes.GenerateDebugAssemblies |
        Microsoft.Scripting.Generation.AssemblyGenAttributes.DisableOptimizations |
        Microsoft.Scripting.Generation.AssemblyGenAttributes.VerifyAssemblies |
#endif
        Microsoft.Scripting.Generation.AssemblyGenAttributes.SaveAndReloadAssemblies;
      ScriptDomainManager.Options.DynamicStackTraceSupport = false;
    }

    protected override void Initialize()
    {
      base.Initialize();
      this.Options.LanguageProvider = ScriptEnvironment.GetEnvironment().GetLanguageProvider(typeof(IronSchemeLanguageProvider));

    }

    protected override void PrintLogo()
    {
      Console.WriteLine("IronScheme {0} http://www.codeplex.com/IronScheme Copyright © leppie 2007", typeof(IronSchemeConsoleHost).Assembly.GetName().Version);
      //Console.WriteLine("Ctrl+Z (or F6) then Enter to quit. On error, hit Enter to return to prompt.");
    }

    protected override void PrintHelp()
    {
      System.Diagnostics.Debugger.Launch();
      base.PrintHelp();
    }

    protected override void UnhandledException(IScriptEngine engine, Exception e)
    {
      base.UnhandledException(engine, e);
    }
  }
}
