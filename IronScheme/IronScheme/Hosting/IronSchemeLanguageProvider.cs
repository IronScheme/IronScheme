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
using Microsoft.Scripting.Shell;

namespace IronScheme.Hosting
{
  class IronSchemeLanguageProvider : LanguageProvider
  {
    public IronSchemeLanguageProvider(ScriptDomainManager x)
      : base(x)
    {
      x.RegisterLanguageProvider("IronScheme", "IronScheme.Hosting.IronSchemeLanguageProvider", ".scm");
    }

    public override string LanguageDisplayName
    {
      get { return "IronScheme"; }
    }

    public override ScriptEngine GetEngine(EngineOptions options)
    {
      LanguageContext lc = new IronSchemeLanguageContext();
      return new IronSchemeScriptEngine(this, options ?? GetOptionsParser().EngineOptions, lc);
    }



    public override CommandLine GetCommandLine()
    {
      CommandLine cl = new CommandLineX();
      
      return cl;
    }

    class CommandLineX : CommandLine
    {
      protected override string Prompt
      {
        get{ return "> ";}
      }

      protected override string PromptContinuation
      {
        get{ return ". ";}
      }

      protected override void OnInteractiveLoopStart()
      {
        this.Engine.Execute("(load \"init.scm\")", Module);
      }
    }

    public override OptionsParser GetOptionsParser()
    {
      return new IronSchemeOptionsParser();
    }

    class IronSchemeOptionsParser : OptionsParser
    {
      public override void GetHelp(out string commandLine, out string[,] options, out string[,] environmentVariables, out string comments)
      {
        commandLine = null;
        options = null;
        environmentVariables = null;
        comments = null;
      }

      class IronSchemeConsoleOptions : ConsoleOptions
      {
      }

      public override Microsoft.Scripting.Shell.ConsoleOptions ConsoleOptions
      {
        get
        {
          ConsoleOptions co = new IronSchemeConsoleOptions();
          co.TabCompletion = true;
          co.ColorfulConsole = true;
          co.AutoIndent = true;
          co.AutoIndentSize = 2;
          co.Introspection = true;
          co.TabCompletion = true;
          return co;
        }
        set
        {
          base.ConsoleOptions = value;
        }
      }

      class IronSchemeEngineOptions : EngineOptions
      {
      }

      public override EngineOptions EngineOptions
      {
        get
        {
          EngineOptions eo = new IronSchemeEngineOptions();
          eo.ClrDebuggingEnabled = true;
          eo.ProfileDrivenCompilation = false;
          return eo;
        }
        set
        {
          base.EngineOptions = value;
        }
      }
    }
  }
}
