#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using IronScheme.Hosting;
using Microsoft.Scripting.Hosting;
using IronScheme.Runtime;
using System.Text.RegularExpressions;
using System.IO;
using System.Reflection;

namespace IronScheme
{
  public static class RuntimeExtensions
  {
    readonly static LanguageProvider provider =
      ScriptDomainManager.CurrentManager.GetLanguageProvider(typeof(IronSchemeLanguageProvider));

    readonly static ScriptEngine se = provider.GetEngine();

    public static ScriptEngine ScriptEngine
    {
      get { return se; }
    } 

    public const string INTERACTION_ENVIRONMENT = "(interaction-environment)";

    public static LanguageProvider Provider
    {
      get { return provider; }
    }

    public static object Eval(this string expr, params object[] args)
    {
      return EvalWithEnvironment(expr, INTERACTION_ENVIRONMENT, args);
    }

    static readonly Regex INDEXREPLACE = new Regex("{(?<index>\\d+)}", RegexOptions.Compiled);

    public static object EvalWithEnvironment(this string expr, string importspec, params object[] args)
    {
      if (string.IsNullOrEmpty(expr))
      {
        throw new ArgumentException("expr cannot be null or empty");
      }
      if (string.IsNullOrEmpty(importspec))
      {
        throw new ArgumentException("importspec cannot be null or empty");
      }

      if (importspec != INTERACTION_ENVIRONMENT)
      {
        importspec = importspec.Replace("(environment", "(environment '(only (ironscheme) define begin symbol-value)");
      }

      var env = se.Evaluate(importspec);

      return EvalWithEnvironmentInstance(expr, env, args);
    }

    public static object EvalWithEnvironmentInstance(this string expr, object env, params object[] args)
    {
      var currentInteractionEnv = se.Evaluate("(interaction-environment)");
      var envId = string.Format("env:{0}", Guid.NewGuid());
      Builtins.SetSymbolValueFast(SymbolTable.StringToObject(envId), env);
      var isInteractive = Builtins.IsTrue(se.Evaluate(string.Format("(interaction-environment? (symbol-value '{0}))", envId)));

      Guid[] replacements = new Guid[args.Length];
      string[] vars = new string[args.Length];

      expr = INDEXREPLACE.Replace(expr, m =>
      {
        Guid g = Guid.NewGuid();
        var index = Convert.ToInt32(m.Groups["index"].Value);
        var p = replacements[index];
        if (p == Guid.Empty)
        {
          replacements[index] = g;
        }
        else
        {
          g = p;
        }
        return vars[index] = string.Format("$eval:{0}", g);
      });

      string[] assigns = new string[args.Length];

      for (int i = 0; i < args.Length; i++)
      {
        var arg = vars[i];
        var rarg = args[i];

        // intern symbols
        if (rarg is SymbolId)
        {
          rarg = SymbolTable.Intern((SymbolId)rarg);
        }

        Builtins.SetSymbolValueFast(SymbolTable.StringToObject(arg), rarg);
        assigns[i] = string.Format("(define {0} (symbol-value '{0}))", arg);
      }

      // must start try here, values have been assigned
      try
      {
        if (assigns.Length > 0)
        {
          expr = string.Format("({2} {0} {1})", string.Join(" ", assigns), expr, isInteractive ? "begin" : "let ()");
        }

        if (!isInteractive || env != currentInteractionEnv)
        {
          expr = string.Format("(eval '{0} (symbol-value '{1}))", expr, envId);
        }

        return se.Evaluate(expr);
      }
      finally
      {
        for (int i = 0; i < args.Length; i++)
        {
          Builtins.RemoveLocation(SymbolTable.StringToObject(vars[i]));
        }

        Builtins.RemoveLocation(SymbolTable.StringToObject(envId));
      }
    }

    public static T Eval<T>(this string expr, params object[] args)
    {
      return EvalWithEnvironment<T>(expr, INTERACTION_ENVIRONMENT, args);
    }

    public static T EvalWithEnvironment<T>(this string expr, string importspec, params object[] args)
    {
      return (T)EvalWithEnvironment(expr, importspec, args);
    }

    public static T EvalWithEnvironmentInstance<T>(this string expr, object env, params object[] args)
    {
      return (T)EvalWithEnvironmentInstance(expr, env, args);
    }

    readonly static Type[] CallTargets = 
    {
      typeof(CallTarget0),
      typeof(CallTarget1),
      typeof(CallTarget2),
      typeof(CallTarget3),
      typeof(CallTarget4),
      typeof(CallTarget5),
      typeof(CallTarget6),
      typeof(CallTarget7),
      typeof(CallTarget8),
    };

    public static Callable ToSchemeProcedure(this Delegate del)
    {
      return del.Method.ToSchemeProcedure(del.Target);
    }

    public static Callable ToSchemeProcedure(this MethodInfo m, object target = null)
    {
      var pars = m.GetParameters();
      var rt = m.ReturnType;
      var partypes = Array.ConvertAll(pars, x => x.ParameterType);

      List<Type> g = new List<Type>();
      g.AddRange(partypes);
      g.Add(rt);

      var allargs = g.ToArray();

      if (Array.TrueForAll(allargs, x => x == typeof(object)))
      {
        return Closure.Create(Delegate.CreateDelegate(CallTargets[partypes.Length], target, m));
      }
      else
      {
        var tc = typeof(Callable).Assembly.GetType("IronScheme.Runtime.Typed.TypedClosure`" + allargs.Length);
        var dt = typeof(Callable).Assembly.GetType("IronScheme.Runtime.Typed.Func`" + allargs.Length);

        tc = tc.MakeGenericType(allargs);
        dt = dt.MakeGenericType(allargs);

        var tci = Activator.CreateInstance(tc, Delegate.CreateDelegate(dt, target, m));

        return tci as Callable;
      }
    }

    public static T ToDelegate<T>(this Callable c)
    {
      return Runtime.Helpers.ConvertToDelegate<T>(c);
    }

    static int Main(string[] args)
    {
      return IronSchemeConsoleHost.Execute(args);
    }
  }
  
  public class ExecutableLoader : MarshalByRefObject
  {
    public ExecutableLoader(Stream s, string[] args)
    {
      using (var r = new StreamReader(s))
      {
        Cons cmdline = Cons.FromList(args);
        RuntimeExtensions.Eval("(apply load-port {0} {1})", r, cmdline);
      }
    }
  }
}
