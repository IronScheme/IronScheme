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
using System.Runtime.Remoting.Channels;
using System.Runtime.Remoting.Channels.Ipc;
using System.Runtime.Remoting;
using IronScheme.Remoting;
using IronScheme.Runtime;
using System.Collections;
using System.Text.RegularExpressions;
using System.Threading;

namespace IronScheme.Hosting
{
  public sealed class IronSchemeConsoleHost : ConsoleHost
  {
    const string VERSION = "1.0 beta 3a";
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

      if (Debugger.IsAttached)
      {
        Console.Title = string.Format("IronScheme {0} - Debugger Attached ", VERSION);
      }
      else
      {
        Console.Title = string.Format("IronScheme {0} ", VERSION);
      }
    }

    abstract class RemoteService : MarshalByRefObject
    {
      protected ILanguageProvider lp;
      protected IScriptEngine se;

      public RemoteService()
      {
        lp = ScriptEnvironment.GetEnvironment().GetLanguageProvider(typeof(IronSchemeLanguageProvider));
        se = lp.GetEngine();
      }

      protected string GetImportSpec(string spec)
      {
        return string.Format("(apply environment '({0}))", spec);
      }

      protected string GetInteractionEnv()
      {
        return RuntimeExtensions.INTERACTION_ENVIRONMENT;
      }

      public override object InitializeLifetimeService()
      {
        return null;
      }

    }

    class RemoteSchemeObject : ObjectHandle, IRemoteSchemeObject
    {
      public RemoteSchemeObject(object value) : base(value)
      {

      }

      public string Type
      {
        get
        {
          object v = Unwrap();
          if (v == null || v is Cons)
          {
            return "pair";
          }
          return v.GetType().ToString();
        }
      }

      public override string ToString()
      {
        return InteractionService.WriteFormat(Unwrap());
      }
    }

    class InteractionService : RemoteService, IInteractionService
    {

      #region IInteractionService Members

      internal static string WriteFormat(object obj)
      {
        var w = new IronScheme.Runtime.StringWriter();
        "(write {0} {1})".Eval(obj, w);
        return w.GetBuffer();
      }

      public string EvalToString(string expr)
      {
        object result = EvalInternal(expr, GetInteractionEnv());
        return WriteFormat(result);
      }

      public string EvalToString(string expr, string importspec)
      {
        object result = EvalInternal(expr, GetImportSpec(importspec));
        return WriteFormat(result);
      }

      object ToRemote(object obj)
      {
        Type t = obj.GetType();

        if (t.Assembly == typeof(object).Assembly && t.IsSerializable)
        {
          return obj;
        }

        return new RemoteSchemeObject(obj);
      }

      object FromRemote(object obj)
      {
        if (obj is RemoteSchemeObject)
        {
          return ((RemoteSchemeObject)obj).Unwrap();
        }
        return obj;
      }

      public object Eval(string expr, params object[] args)
      {
        object result = EvalInternal(expr, GetInteractionEnv(), Array.ConvertAll(args, x => FromRemote(x)));
        return ToRemote(result);
      }

      public object Eval(string expr, string importspec, params object[] args)
      {
        object result = EvalInternal(expr, GetImportSpec(importspec), Array.ConvertAll(args, x => FromRemote(x)));
        return ToRemote(result);
      }

      object EvalInternal(string expr, string importspec, params object[] args)
      {

        try
        {
          return expr.Eval(importspec, args);
        }
        catch (Exception ex)
        {
          throw new EvaluationException(ex.ToString());
        }
      }

      #endregion
    }

    class SBService : RemoteService, ISymbolBindingService
    {
      public SymbolBinding[] GetBindings(string importspec)
      {
        return GetBindingsInternal(GetImportSpec(importspec));
      }

      public SymbolBinding[] GetBindings()
      {
        return GetBindingsInternal(GetImportSpec("(ironscheme)"));
      }


      SymbolBinding[] GetBindingsInternal(string importspec)
      {
        if (string.IsNullOrEmpty(importspec))
        {
          throw new ArgumentException("importspec cannot be null or empty");
        }
        try
        {
          ICallable c = se.Evaluate(string.Format(@"
(lambda (maker)
  (map (lambda (b) 
          (maker (car b) (cdr b)))
       (environment-bindings {0})))
", importspec)) as ICallable;

          CallTarget2 maker = (n, t) =>
            {
              return new SymbolBinding
              {
                Name = SymbolTable.IdToString((SymbolId)n),
                Type = (BindingType)Enum.Parse(typeof(BindingType), SymbolTable.IdToString((SymbolId)t), true),
              };
            };

          var r = c.Call(Closure.MakeStatic(maker)) as IEnumerable;

          List<SymbolBinding> sbs = new List<SymbolBinding>();

          foreach (SymbolBinding sb in r)
          {
            sbs.Add(sb);
          }

          return sbs.ToArray();
        }
        catch (Exception ex)
        {
          throw new EvaluationException(ex.ToString());
        }
      }


      ProcedureInfo GetProcedureInfoInternal(string proc, string importspec)
      {
        if (string.IsNullOrEmpty(proc))
        {
          throw new ArgumentException("proc cannot be null or empty");
        }
        if (string.IsNullOrEmpty(importspec))
        {
          throw new ArgumentException("importspec cannot be null or empty");
        }

        try
        {
          ICallable c = se.Evaluate(string.Format(@"
(lambda (maker)
  (let ((p (eval '{0} {1})))
    (let ((forms (call-with-values (lambda () (procedure-form p)) vector)))
      (maker (symbol->string '{0}) forms))))
", proc, importspec)) as ICallable;

          CallTarget2 maker = (n, forms) =>
            {
              var f = forms as object[];
              List<string> ff = new List<string>();
              foreach (Cons fc in f)
              {
                ff.Add(fc.PrettyPrint.TrimEnd('\n'));
              }

              var pi = new ProcedureInfo
              {
                Name = n as string,
                Forms = ff.ToArray()
              };
              return pi;
            };

          return c.Call(Closure.MakeStatic(maker)) as ProcedureInfo;
        }
        catch (Exception ex)
        {
          throw new EvaluationException(ex.ToString());
        }
      }

      public ProcedureInfo GetProcedureInfo(string proc)
      {
        return GetProcedureInfoInternal(proc, GetInteractionEnv());
      }

      public ProcedureInfo GetProcedureInfo(string proc, string importspec)
      {
        return GetProcedureInfoInternal(proc, GetImportSpec(importspec));
      }
    }

    Mutex m;

    protected override void Initialize()
    {
      base.Initialize();
      Options.LanguageProvider = ScriptEnvironment.GetEnvironment().GetLanguageProvider(typeof(IronSchemeLanguageProvider));

      // only on first instance for now
      bool created;
      m = new Mutex(true, "IronScheme", out created);

      if (created)
      {
        BinaryServerFormatterSinkProvider sbs = new BinaryServerFormatterSinkProvider();
        sbs.TypeFilterLevel = System.Runtime.Serialization.Formatters.TypeFilterLevel.Full;

        IpcServerChannel server = new IpcServerChannel("IronScheme", "IronScheme", sbs);

        ChannelServices.RegisterChannel(server, false);

        RemotingConfiguration.RegisterWellKnownServiceType(typeof(SBService), "SymbolBindingService", WellKnownObjectMode.Singleton);
        RemotingConfiguration.RegisterWellKnownServiceType(typeof(InteractionService), "InteractionService", WellKnownObjectMode.Singleton);

        Console.Title += "- Remote Host ";
      }

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
