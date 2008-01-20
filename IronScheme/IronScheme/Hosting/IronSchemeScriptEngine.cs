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

    protected override void PrintInteractiveCodeResult(object obj)
    {
      if (obj != Builtins.Unspecified)
      {
        string strv = Builtins.DisplayFormat(obj);
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
&message:      continuations cannot be used in this way, sorry :(";
      }
      if (exception is NotSupportedException)
      {
        return @"&implementation-restriction
&message:      " + exception.Message;
      }
      if (exception is ArgumentTypeException || exception is ArgumentNullException || exception is ArgumentException || exception is ArgumentOutOfRangeException)
      {
        return "argument error: " + exception.Message;
      }
      if (exception is ThreadAbortException)
      {
        return "evaluation aborted";
      }
      if (exception is InvalidCastException)
      {
        return @"&assertion
&message:      " + exception.Message;
      }
      return base.FormatException(exception);
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
        n[i] = names[i] as string;
      }

      return n;
    }

    protected override IList<object> Ops_GetAttrNames(CodeContext context, object obj)
    {
      return Identifiers.GetR6RSIds();
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

    #endregion

    internal CodeContext CreateContext(ModuleContext mc)
    {
      return new CodeContext(new Scope(), LanguageContext, mc);
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
        if (sourceUnit.Kind == SourceCodeKind.InteractiveCode && message != "unexpected EOF")
        {
          Builtins.LexicalError(message, false);
          throw new SyntaxErrorException(message, sourceUnit, span, errorCode, severity);
        }
        else
        if (sourceUnit.Kind != SourceCodeKind.InteractiveCode)
        {
          Builtins.LexicalError(message, false);
          throw new SyntaxErrorException(message, sourceUnit, span, errorCode, severity);
        }
      }
    }
  }
}
