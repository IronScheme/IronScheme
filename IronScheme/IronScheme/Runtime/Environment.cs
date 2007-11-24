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
using Microsoft.Scripting.Math;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Collections;
using Microsoft.Scripting;
using System.IO;
using Microsoft.Scripting.Hosting;
using System.Diagnostics;

namespace IronScheme.Runtime
{
  public class SchemeException : Exception
  {
    public SchemeException(string msg) : base(msg)
    {

    }
  }

  public partial class Builtins
  {

    [Builtin("interaction-environment")]
    public static object InteractionEnvironment()
    {
      return false;
    }

    [Builtin("null-environment")]
    public static object NullEnvironment(object version)
    {
      return false;
    }

    [Builtin("scheme-report-environment")]
    public static object SchemeReportEnvironment(object version)
    {
      return false;
    }

    [Builtin("error")]
    public static object Error(object reason, params object[] errors)
    {
      string[] ll = Array.ConvertAll<object, string>(errors, delegate(object o) { return DisplayFormat(o); });
      ll = ArrayUtils.Insert<string>(DisplayFormat(reason), ll);
      throw new SchemeException(string.Join(", ", ll));
    }



    [Builtin("eval-string")]
    public static object EvalString(CodeContext cc, string expr)
    {
      SourceUnit su = SourceUnit.CreateSnippet(ScriptEngine, expr, SourceCodeKind.Expression);
      ScriptCode sc = cc.LanguageContext.CompileSourceCode(su);

      object result = sc.Run(cc.Scope, cc.ModuleContext);
      return result;
    }

    static int symcount = 0;

    [Builtin]
    public static object GenSym()
    {
      return SymbolTable.StringToId("g$" + symcount++);
    }

    [Builtin]
    public static object GenSym(object name)
    {
      if (name is string)
      {
        string s = RequiresNotNull<string>(name);
        return SymbolTable.StringToId("g$" + s + "$" + symcount++);
      }
      else
      {
        SymbolId s = RequiresNotNull<SymbolId>(name);
        return SymbolTable.StringToId("g$" + s + "$" + symcount++);
      }
    }

    [Builtin]
    public static object Eval(CodeContext cc, object expr)
    {
      Cons list = expr as Cons;
      if (list != null)
      {
        // we could modify the parser to accept this cons perhaps? this is easy and cheap for now
        string exprstr = WriteFormat(list);
        return EvalString(cc, exprstr);
      }
      if (expr is SymbolId)
      {
        return cc.LanguageContext.LookupName(cc, (SymbolId)expr);
      }

      return expr;
    }

  }
}
