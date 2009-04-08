using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using IronScheme.Hosting;
using Microsoft.Scripting.Hosting;
using IronScheme.Runtime;

namespace IronScheme
{
  public static class RuntimeExtensions
  {
    readonly static LanguageProvider provider =
      ScriptDomainManager.CurrentManager.GetLanguageProvider(typeof(IronSchemeLanguageProvider));

    public static LanguageProvider Provider
    {
      get { return provider; }
    }

    public static object EvalInScheme(this string expr)
    {
      return provider.GetEngine().Evaluate(expr);
    }

    public static T EvalInScheme<T>(this string expr)
    {
      return (T)EvalInScheme(expr);
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
