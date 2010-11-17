#region License
/* Copyright (c) 2007,2008,2009,2010 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.IO;
using System.Threading;
using IronScheme.Runtime;
using Microsoft.Scripting;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Shell;

namespace IronScheme.Hosting
{
  public sealed class IronSchemeLanguageProvider : LanguageProvider
  {
    public IronSchemeLanguageProvider() : this (ScriptDomainManager.CurrentManager)
    {
    }

    public IronSchemeLanguageProvider(ScriptDomainManager x)
      : base(x)
    {
      ScriptDomainManager.Options.DynamicStackTraceSupport = false;
      
      Runtime.Closure.ConsFromArray = Runtime.Cons.FromArray;
      Runtime.Closure.ConsStarFromArray = delegate(object[] args) { return Builtins.ToImproper(Cons.FromArray(args)); };
      Runtime.Closure.Unspecified = Builtins.Unspecified;
      Runtime.Closure.ArrayFromCons = l => (object[])Builtins.ListToVector((Cons)l);

      Initialize();

      // only register when done
      x.RegisterLanguageProvider("IronScheme", "IronScheme.Hosting.IronSchemeLanguageProvider", ".sps", ".ss", ".sls");
    }

    void Initialize()
    {
      IronScheme.Compiler.BaseHelper.Initialize(this);
      Runtime.Builtins.Load("~/ironscheme.boot.dll");
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

    public override IConsole GetConsole(CommandLine commandLine, IScriptEngine engine, ConsoleOptions options)
    {
      return base.GetConsole(commandLine, engine, options);
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
        get { return "> "; }
      }

      internal IConsole GetConsole()
      {
        return Console;
      }

      protected override string PromptContinuation
      {
        get { return ". "; }
      }

      protected override int RunFile(string filename)
      {
        var rest = Options.RemainingArgs;
        for (int i = 0; i < rest.Length; i++)
        {
          if (rest[i] == filename)
          {
            var tail = new string[rest.Length - i];
            Array.Copy(rest, i, tail, 0, tail.Length);

            tail[0] = tail[0].Replace("\\", "/");

            Engine.Execute(string.Format("(command-line '(\"{0}\"))", string.Join("\" \"", tail)), 
              Compiler.BaseHelper.scriptmodule);
          }
        }

        if (!Options.TabCompletion)
        {
          Engine.Execute("(emacs-mode? #t)", Compiler.BaseHelper.scriptmodule);
        }
        int ev = 0;
        AutoResetEvent e = new AutoResetEvent(false);
        Thread t = new Thread(delegate ()
          {
            try
            {
#if CPS
              Engine.Execute(string.Format("(load identity-for-cps \"{0}\")", filename.Replace('\\', '/')));
#else
              Engine.Execute(string.Format("(load \"{0}\")", filename.Replace('\\', '/')));
#endif
              ev = 0;
            }
            catch (ThreadAbortException)
            {
              Console.WriteLine("Execution interupted", Style.Error);
              ev = 2;
            }
            catch (Exception ex)
            {
              var fmt = Engine.FormatException(ex);
              if (fmt != null)
              {
                Console.WriteLine(fmt, Style.Error);
              }
              ev = 1;
            }
            finally
            {
              e.Set();
            }
            //good for now

          }, 
#if CPS //to compile...
          10000000
#else
          5000000
#endif
          );
        t.IsBackground = true;

        t.Start();

        System.Console.CancelKeyPress += delegate
        {
          t.Abort();
          System.Console.Error.WriteLine("Evaluation aborted.");
          System.Environment.Exit(1);
        };

        e.WaitOne();

        return ev;
      }

      protected override void OnInteractiveLoopStart()
      {
        IronScheme.Runtime.Builtins.commandline = new string[] { "interactive" };



        if (!Options.TabCompletion)
        {
          Engine.Execute("(emacs-mode? #t)", Compiler.BaseHelper.scriptmodule);
        }

        if (File.Exists("init.ss"))
        {
          Engine.Execute("(include \"init.ss\")", Compiler.BaseHelper.scriptmodule);
        }



      }
    }

    public override OptionsParser GetOptionsParser()
    {
      return new IronSchemeOptionsParser();
    }

    class IronSchemeOptionsParser : OptionsParser
    {
      public IronSchemeOptionsParser() : base()
      {
        notabcompletion = IsNotConsole();
      }

      public override void GetHelp(out string commandLine, out string[,] options, out string[,] environmentVariables, out string comments)
      {
        commandLine = null;
        options = null;
        environmentVariables = null;
        comments = null;
      }

      bool notabcompletion = false;

      static bool IsNotConsole()
      {
        return System.Console.LargestWindowHeight == 0 && System.Console.LargestWindowWidth == 0 || LanguageProvider.InputRedirected;
      }

      protected override void ParseArgument(string arg)
      {
        if (arg == "-notabcompletion" || arg == "-emacs")
        {
          notabcompletion = true;
        }
        else if (arg == "-I")
        {
          var includepath = PopNextArg();
          Builtins.includepaths.Add(includepath);
        }
        else
        {
          base.ParseArgument(arg);
        }
      }

      class IronSchemeConsoleOptions : ConsoleOptions
      {
      }

      ConsoleOptions co;

      public override Microsoft.Scripting.Shell.ConsoleOptions ConsoleOptions
      {
        get
        {
          if (co == null)
          {
            co = new IronSchemeConsoleOptions();
            co.TabCompletion = !notabcompletion;
            co.ColorfulConsole = !notabcompletion;
          }
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
