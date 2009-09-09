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
using Microsoft.Scripting.Actions;
using IronScheme.Actions;
using System.Reflection;
using System.Collections;
using IronScheme.Runtime;
using System.Diagnostics;
using Microsoft.Scripting.Shell;
using System.Threading;
using System.IO;

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
#if !CPS
      if (exception is Builtins.Continuation)
      {
        // cheat
        return @"&implementation-restriction
&message:             continuations cannot be used in this way, sorry :(";
      }
#endif
      if (exception is NotSupportedException)
      {
        return @"&implementation-restriction
&message:             " + exception.Message;
      }

      if (exception is ThreadAbortException)
      {
        return "evaluation aborted";
      }
      if (exception is InvalidCastException)
      {
        return @"&assertion
&message:             " + exception.Message;
      }
      if (exception is Runtime.R6RS.Condition)
      {
        var w = new IronScheme.Runtime.StringWriter();
        "(display {0} {1})".Eval(exception, w);
        return w.GetBuffer();
      }
      return string.Format(@"&clr
&who:                 {3}.{2}
&clr-type:            {0}
&message:             {1}", exception.GetType(), exception.Message, exception.TargetSite.Name,
                        exception.TargetSite.DeclaringType == null ? "<anon>" : exception.TargetSite.DeclaringType.Name);

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
#if CPS
      Cons ids = c.Call(Closure.IdentityForCPS) as Cons;
#else
      Cons ids = c.Call() as Cons;
#endif

      List<object> names = new List<object>(ids);

      names.Sort(delegate(object o, object p) { return o.ToString().CompareTo(p.ToString()); });

      return names;
    }

    public override string Copyright
    {
      get
      {
        return "leppie (c) 2008";
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

    public override Microsoft.Scripting.Hosting.ErrorSink GetCompilerErrorSink()
    {
      return new ErrorSink();
    }

    class ErrorSink : Microsoft.Scripting.Hosting.ErrorSink
    {
      public override void Add(SourceUnit sourceUnit, string message, SourceSpan span, int errorCode, Severity severity)
      {
        base.Add(sourceUnit, message, span, errorCode, severity);
        //if (sourceUnit.Kind == SourceCodeKind.InteractiveCode && message != "unexpected EOF")
        //{
        //  throw new SyntaxErrorException(message, sourceUnit, span, errorCode, severity);
        //}
        //else
        //if (sourceUnit.Kind != SourceCodeKind.InteractiveCode)
        //{
        //  throw new SyntaxErrorException(message, sourceUnit, span, errorCode, severity);
        //}
      }
    }
  }
}
