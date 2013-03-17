#region License
/* Copyright (c) 2007-2013 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.Reflection;
using System.Threading;
using IronScheme.Runtime;
using Microsoft.Scripting;
using Microsoft.Scripting.Actions;
using Microsoft.Scripting.Hosting;
using Microsoft.Scripting.Shell;

namespace IronScheme.Hosting
{
  sealed class IronSchemeScriptEngine : ScriptEngine
  {
    public IronSchemeScriptEngine(LanguageProvider lp, EngineOptions eo, LanguageContext lc)
      : base(lp, eo, lc)
    {
      ((IronSchemeLanguageContext)LanguageContext).se = this;
    }

    public IConsole IConsole
    {
      get { return (LanguageProvider as IronSchemeLanguageProvider).Console; }
    }

    static string WriteFormat(object obj)
    {
      var w = new IronScheme.Runtime.StringWriter();
      "(write {0} {1})".Eval(obj, w);
      return w.GetBuffer();
    }

    protected override void PrintInteractiveCodeResult(object obj)
    {
      if (obj != Builtins.Unspecified)
      {
        string strv = WriteFormat(obj);
        IConsole.WriteLine(string.Format("{0}", strv), Style.Out);
      }
    }

    public override string FormatException(Exception exception)
    {
      while (exception is TargetInvocationException)
      {
        exception = exception.InnerException;
      }
      Builtins.lastException = exception;

      if (exception is Builtins.Continuation)
      {
        // cheat
        return @"&implementation-restriction
&message:             continuations cannot be used in this way, sorry :(";
      }

      if (exception is ThreadAbortException)
      {
        return "evaluation aborted";
      }

      if (exception is SchemeException)
      {
        return exception.ToString();
      }

      var w = new IronScheme.Runtime.StringWriter();
      w.WriteLine("Unhandled CLR exception reading input:");
      "(display {0} {1})".Eval(exception, w);
      return w.GetBuffer();
    }

    #region Abstract


    public override ActionBinder DefaultBinder
    {
      get { return Compiler.Generator.binder; }
    }

    #endregion

    protected override LanguageContext GetLanguageContext(CompilerOptions compilerOptions)
    {
      return LanguageContext;
    }

    #region Virtual

    protected override string[] FormatObjectMemberNames(IList<object> names)
    {

      string[] n = new string[names.Count];
      for (int i = 0; i < names.Count; i++)
      {
        n[i] = string.Format("{0}", names[i]);
      }

      return n;
    }

    protected override IList<object> Ops_GetAttrNames(CodeContext context, object obj)
    {
      Callable c = context.Scope.LookupName(SymbolTable.StringToId("int-env-syms")) as Callable;
      Cons ids = c.Call() as Cons;

      List<object> names = new List<object>(ids);

      names.Sort(delegate(object o, object p) { return o.ToString().CompareTo(p.ToString()); });

      return names;
    }

    public override string Copyright
    {
      get
      {
        return "leppie (c) 2009";
      }
    }

    public override string VersionString
    {
      get
      {
        return "1.0.0";
      }
    }

    public override Guid LanguageGuid
    {
      get
      {
        return new Guid("4BFAEA21-B66A-458b-BC32-24457C9178B8");
      }
    }

    #endregion

    internal LanguageContext GetLanguageContext()
    {
      return LanguageContext;
    }
  }
}
