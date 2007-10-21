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
using IronScheme.Hosting;
using Microsoft.Scripting;

namespace IronSchemeConsole
{
  class Program
  {

    static int Main(string[] args)
    {
//      ScriptDomainManager.CurrentManager.RegisterLanguageProvider("IronScheme", "IronScheme.Hosting.IronSchemeLanguageProvider", ".scm");
//#if DEBUG
//      ScriptDomainManager.Options.AssemblyGenAttributes |=
//        Microsoft.Scripting.Generation.AssemblyGenAttributes.EmitDebugInfo |
//        Microsoft.Scripting.Generation.AssemblyGenAttributes.GenerateDebugAssemblies |
//        Microsoft.Scripting.Generation.AssemblyGenAttributes.VerifyAssemblies |
//        Microsoft.Scripting.Generation.AssemblyGenAttributes.DisableOptimizations |
//        Microsoft.Scripting.Generation.AssemblyGenAttributes.SaveAndReloadAssemblies;
//      //ScriptDomainManager.Options.DumpASTs = true;
//      //ScriptDomainManager.Options.OptimizeEnvironments = false ;
//#endif
//      ScriptDomainManager.Options.DynamicStackTraceSupport = false;
//      //ScriptDomainManager.Options.Frames = true;

//      //Script.Evaluate(".ls", "(prl \"hello world\")");
//      Script.ExecuteFile("test.scm");
//      //Script.Execute(".ls", "(prl 1)");
//      //Script.ExecuteFileContent("test.ls");

//      //Console.ReadLine();
//      return 0;

      return new IronSchemeConsoleHost().Run(args);
    }
  }
}
