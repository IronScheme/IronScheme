#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
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
    internal IronSchemeLanguageProvider()
      : this(ScriptDomainManager.CurrentManager)
    {
    }

    internal IronSchemeLanguageProvider(ScriptDomainManager x)
      : base(x)
    {
      Closure.ConsFromArray = Cons.FromArray;
      Closure.ConsStarFromArray = delegate(object[] args) { return Builtins.ToImproper(Cons.FromArray(args)); };
      Closure.Unspecified = Builtins.Unspecified;
      Closure.ArrayFromCons = Builtins.ListToVector;

      RuntimeHelpers.Unspecified = typeof(Builtins).GetField(nameof(Builtins.Unspecified));

      Initialize();

      // only register when done
      x.RegisterLanguageProvider("IronScheme", "IronScheme.Hosting.IronSchemeLanguageProvider", ".sps", ".ss", ".sls");
    }

    void Initialize()
    {
      //PAL.Initialize();
      BaseHelper.Initialize(this);
      Runtime.Builtins.Load("~/ironscheme.boot.dll", false);
    }
    
    public override string LanguageDisplayName
    {
      get { return "IronScheme"; }
    }

    IronSchemeScriptEngine se;

    public override ScriptEngine GetEngine()
    {
      if (se == null)
      {
        LanguageContext lc = new IronSchemeLanguageContext();
        se = new IronSchemeScriptEngine(this, lc);

      }
      return se;
    }

    public override IConsole GetConsole(CommandLine commandLine, IScriptEngine engine, ConsoleOptions options)
    {
      return base.GetConsole(commandLine, engine, options);
    }

    // make this lazy
    CommandLineX cl;

    void InitializeCommandLine()
    {
      if (cl == null)
      {
         cl = new CommandLineX();
      }
    }

    public IConsole Console
    {
      get
      {
        InitializeCommandLine();
        return cl.GetConsole();
      }

    }

    public override CommandLine GetCommandLine()
    {
      InitializeCommandLine();
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
        if (!File.Exists(filename.Replace('\\', '/')))
        {
          return 1;
        }

        var rest = Options.RemainingArgs;
        for (int i = 0; i < rest.Length; i++)
        {
          if (rest[i] == filename)
          {
            var tail = new string[rest.Length - i];
            Array.Copy(rest, i, tail, 0, tail.Length);

            tail[0] = tail[0].Replace("\\", "/");

            Engine.Execute(string.Format("(command-line '(\"{0}\"))", string.Join("\" \"", tail)), 
              BaseHelper.scriptmodule);
          }
        }

        if (!Options.TabCompletion)
        {
          Engine.Execute("(emacs-mode? #t)", BaseHelper.scriptmodule);
        }
        int ev = 0;
        AutoResetEvent e = new AutoResetEvent(false);
        Thread t = new Thread(delegate ()
          {
            try
            {
              Engine.Execute(string.Format("(load/unsafe \"{0}\")", filename.Replace('\\', '/')));
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
          5000000);
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
        Builtins.commandline = new string[] { "interactive" };

        if (!Options.TabCompletion)
        {
          Engine.Execute("(emacs-mode? #t)", BaseHelper.scriptmodule);
        }

        var nuget = Path.Combine(Builtins.ApplicationDirectory, "nuget.sls");

        if (File.Exists(nuget) && nuget.Contains(Path.DirectorySeparatorChar + "ironscheme.tool" + Path.DirectorySeparatorChar))
        {
          Engine.Execute("(import (nuget)) (unfuck)", BaseHelper.scriptmodule);
          File.Delete(nuget);
        }

        if (File.Exists("init.ss"))
        {
          Engine.Execute("(include \"init.ss\")", BaseHelper.scriptmodule);
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

      public override ConsoleOptions ConsoleOptions
      {
        get
        {
          if (co == null)
          {
            co = new IronSchemeConsoleOptions();
            co.TabCompletion = !notabcompletion;
            co.ColorfulConsole = !notabcompletion;
          }
          return co;
        }
        set
        {
          base.ConsoleOptions = value;
        }
      }
    }
  }
}
