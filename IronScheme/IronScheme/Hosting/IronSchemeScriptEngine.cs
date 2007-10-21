#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Permissive License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Permissive License.
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
      string strv = Builtins.DisplayFormat(obj);
      Console.WriteLine("{0}", strv);
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
      return base.FormatException(exception);
    }

    #region Abstract


    public override ActionBinder DefaultBinder
    {
      get { return new IronSchemeActionBinder(null); }
    }

    #endregion

    protected override LanguageContext GetLanguageContext(CompilerOptions compilerOptions)
    {
      return LanguageContext;
    }

    #region Virtual

    public override string Copyright
    {
      get
      {
        return "leppie 2007";
      }
    }

    public override string VersionString
    {
      get
      {
        return "0.0.1";
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
