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
        Microsoft.Scripting.Generation.AssemblyGenAttributes.EmitDebugInfo |
        Microsoft.Scripting.Generation.AssemblyGenAttributes.GenerateDebugAssemblies |
        //Microsoft.Scripting.Generation.AssemblyGenAttributes.VerifyAssemblies |
        //Microsoft.Scripting.Generation.AssemblyGenAttributes.DisableOptimizations | 
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
      Console.WriteLine("IronScheme {0} (c)2007 llewellyn@pritchard.org http://www.codeplex.com/IronScheme", typeof(IronSchemeConsoleHost).Assembly.GetName().Version);
      Console.WriteLine("Ctrl+Z (or F6) then Enter to quit. On error, hit Enter to return to prompt.");
    }
  }
}
