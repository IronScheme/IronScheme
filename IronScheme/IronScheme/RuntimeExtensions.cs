using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using IronScheme.Hosting;
using Microsoft.Scripting.Hosting;
using IronScheme.Runtime;
using System.Text.RegularExpressions;

namespace IronScheme
{
  public static class RuntimeExtensions
  {
    readonly static LanguageProvider provider =
      ScriptDomainManager.CurrentManager.GetLanguageProvider(typeof(IronSchemeLanguageProvider));

    readonly static ScriptEngine se = provider.GetEngine();

    internal const string INTERACTION_ENVIRONMENT = "(interaction-environment)";

    public static LanguageProvider Provider
    {
      get { return provider; }
    }

    public static object Eval(this string expr, params object[] args)
    {
      return Eval(expr, INTERACTION_ENVIRONMENT, args);
    }

    public static object Eval(this string expr, string importspec, params object[] args)
    {
      if (string.IsNullOrEmpty(expr))
      {
        throw new ArgumentException("expr cannot be null or empty");
      }
      if (string.IsNullOrEmpty(importspec))
      {
        throw new ArgumentException("importspec cannot be null or empty");
      }

      Guid[] replacements = new Guid[args.Length];
      string[] vars = new string[args.Length];

      expr = Regex.Replace(expr, "{(?<index>\\d+)}", m =>
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
        Builtins.SetSymbolValue(SymbolTable.StringToObject(arg), args[i]);
        assigns[i] = string.Format("({0} (symbol-value '{0}))", arg);
      }

      // must start try here, values have been assigned
      try
      {
        expr = string.Format("(let ({0}) {1})", string.Join(" ", assigns), expr);

        if (importspec != INTERACTION_ENVIRONMENT)
        {
          expr = string.Format("(eval '{0} {1})", expr, importspec);
        }

        return se.Evaluate(expr);
      }
      finally
      {
        for (int i = 0; i < args.Length; i++)
        {
          Builtins.RemoveLocation(SymbolTable.StringToObject(vars[i]));
        }
      }
    }

    public static T Eval<T>(this string expr, params object[] args)
    {
      return (T)Eval(expr, INTERACTION_ENVIRONMENT, args);
    }

    public static T Eval<T>(this string expr, string importspec, params object[] args)
    {
      return (T)Eval(expr, importspec, args);
    }


#pragma warning disable 3001,3002

    public static ICallable ToSchemeProcedure(this Delegate del)
    {
      return Closure.MakeStatic(del);
    }

    public static T ToDelegate<T>(this ICallable c)
    {
      return Runtime.Helpers.ConvertToDelegate<T>(c);
    }

#pragma warning restore 3001,3002
  }
}
