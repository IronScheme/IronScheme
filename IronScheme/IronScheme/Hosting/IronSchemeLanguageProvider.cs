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
using Microsoft.Scripting.Shell;
using System.IO;

namespace IronScheme.Hosting
{
  sealed class IronSchemeLanguageProvider : LanguageProvider
  {
    public IronSchemeLanguageProvider(ScriptDomainManager x)
      : base(x)
    {
      x.RegisterLanguageProvider("IronScheme", "IronScheme.Hosting.IronSchemeLanguageProvider", ".scm", ".ss", ".sch");
      Runtime.Closure.ConsFromArray = Runtime.Cons.FromArray;
    }

    public override string LanguageDisplayName
    {
      get { return "IronScheme"; }
    }

    IronSchemeScriptEngine se;

    public override ScriptEngine GetEngine(EngineOptions options)
    {
      if (se == null)
      {
        LanguageContext lc = new IronSchemeLanguageContext();
        se = new IronSchemeScriptEngine(this, options ?? GetOptionsParser().EngineOptions, lc);
      }
      return se;
    }

    CommandLineX cl = new CommandLineX();

    public IConsole Console
    {
      get
      {
        return cl.GetConsole();
      }

    }

    public override CommandLine GetCommandLine()
    {
      return cl; 
    }

    class CommandLineX : CommandLine
    {
      protected override string Prompt
      {
        get{ return "> ";}
      }

      internal IConsole GetConsole()
      {
        return Console;
      }

      protected override string PromptContinuation
      {
        get{ return ". ";}
      }

      protected override int RunFile(string filename)
      {
        return base.RunFile(filename);
      }

      protected override int RunCommand(string command)
      {
        return base.RunCommand(command);
      }

      protected override void Initialize()
      {
        if (File.Exists("unknown_ast.ast"))
        {
          File.Delete("unknown_ast.ast");
        }
        base.Initialize();
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

      bool notabcompletion = false;

      protected override void ParseArgument(string arg)
      {
        if (arg == "-notabcompletion")
        {
          notabcompletion = true;
        }
        else
        {
          base.ParseArgument(arg);
        }
      }

      class IronSchemeConsoleOptions : ConsoleOptions
      {
      }

      public override Microsoft.Scripting.Shell.ConsoleOptions ConsoleOptions
      {
        get
        {
          ConsoleOptions co = new IronSchemeConsoleOptions();
          co.TabCompletion = !notabcompletion;
          co.ColorfulConsole = true;
#if DEBUG
          //co.HandleExceptions = false;
#endif
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
          eo.ProfileDrivenCompilation = false;
          //eo.InterpretedMode = true;
          // this will blow up visual studio with anything but small files
          //eo.ClrDebuggingEnabled = true;
          
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
