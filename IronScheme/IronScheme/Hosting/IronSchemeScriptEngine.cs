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

namespace IronScheme.Hosting
{
  public class IronSchemeScriptEngine : ScriptEngine
  {
    public IronSchemeScriptEngine(LanguageProvider lp, EngineOptions eo, LanguageContext lc)
      : base(lp, eo, lc)
    {
      ((IronSchemeLanguageContext)LanguageContext).se = this;
    }

    protected override void PrintInteractiveCodeResult(object obj)
    {
      if (obj != Builtins.Unspecified)
      {
        string strv = Builtins.DisplayFormat(obj);
        Console.WriteLine("{0}", strv);
      }
    }

    public override string FormatException(Exception exception)
    {
      if (exception is MissingMemberException)
      {
        return string.Format("error: {0}", exception.Message);
      }
      if (exception is SyntaxErrorException)
      {
        SyntaxErrorException se = (SyntaxErrorException)exception;
        return string.Format("{0} error: {1} at ({2}:{3})", se.ErrorCode == 2 ? "lexer" : "parser", se.Message, se.Line, se.Column);
      }
      if (exception is Builtins.Continuation)
      {
        return "not supported: continuations cannot be used in this way, sorry :(";
      }
      if (exception is NotSupportedException)
      {
        return "not supported: " + exception.Message;
      }
      if (exception is NotImplementedException)
      {
        return "not implemented: " + exception.Message;
      }
      if (exception is ArgumentTypeException || exception is ArgumentNullException || exception is ArgumentException || exception is ArgumentOutOfRangeException)
      {
        return "argument error: " + exception.Message;
      }
      if (exception is ArgumentNullException)
      {
        //return "argument error: " + 
      }
      return base.FormatException(exception);
    }

    #region Abstract


    public override ActionBinder DefaultBinder
    {
      get { return Compiler.Generator.BINDER; }
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
      Array.Sort(n);
      return n;
    }

    protected override IList<object> Ops_GetAttrNames(CodeContext context, object obj)
    {
      List<object> ll = new List<object>();
      foreach (SymbolId var in Compiler.Generator.Compiler.Scope.Keys)
      {
        ll.Add(SymbolTable.IdToString(var));
      }

      return ll;
       
      //return base.Ops_GetAttrNames(context, obj);
    }

    public override string Copyright
    {
      get
      {
        return "leppie (c) 2007";
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
        throw new SyntaxErrorException(message, sourceUnit, span, errorCode, severity);
      }
    }
  }
}
