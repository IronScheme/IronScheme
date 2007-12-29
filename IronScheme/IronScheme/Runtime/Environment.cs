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
    readonly string who, message;
    readonly string[] irritants;

    public SchemeException(string who, string message, string[] irritants) : base()
    {
      this.who = who;
      this.message = message;
      this.irritants = irritants;
    }

    static string FormatMessage(string msg, params string[] args)
    {
      int s = 0, i = 0;
      while ((s = msg.IndexOf("~s", s)) >= 0)
      {
        msg = msg.Substring(0, s) + args[i] + msg.Substring(s + 2);
      }

      return msg;

    }

    public override string Message
    {
      get
      {
        List<string> ii = new List<string>();
        for (int i = 0; i < irritants.Length; i++)
        {
          ii.Add(string.Format("    [{0}]\t{1}", i, irritants[i]));
        }
        return string.Format(@"error!
who:        {0}
message:    {1}
irritants:
{2}", who, FormatMessage(message, irritants), string.Join(Environment.NewLine, ii.ToArray()));
      }
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

#if R6RS

    public static object UndefinedError(object sym)
    {
      ICallable u = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&undefined-rcd"))) as ICallable;
      ICallable i = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&irritants-rcd"))) as ICallable;

      R6RS.Exceptions.RaiseContinueable(
        R6RS.Conditions.Condition(u.Call(), i.Call(List(sym))));

      return Unspecified;
    }

    public static object LexicalError(string msg, object what)
    {
      ICallable l = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&lexical-rcd"))) as ICallable;
      ICallable m = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&message-rcd"))) as ICallable;
      ICallable i = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&irritants-rcd"))) as ICallable;

      R6RS.Exceptions.RaiseContinueable(
        R6RS.Conditions.Condition(l.Call(), m.Call(msg), i.Call(List(what))));

      return Unspecified;
    }

    public static object SyntaxError(object who, object message, object form, object subform)
    {
      ICallable s = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&syntax-rcd"))) as ICallable;
      ICallable w = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&who-rcd"))) as ICallable;
      ICallable m = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&message-rcd"))) as ICallable;

      if (who is bool && !(bool)who)
      {
        R6RS.Exceptions.RaiseContinueable(
          R6RS.Conditions.Condition( m.Call(message), s.Call(form, subform)));
      }
      else
      {
        R6RS.Exceptions.RaiseContinueable(
          R6RS.Conditions.Condition(w.Call(who), m.Call(message), s.Call(form, subform)));
      }

      return Unspecified;
    }


    [Builtin("assertion-violation")]
    public static object AssertionViolation(object who, object message, params object[] irritants)
    {
      ICallable a = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&assertion-rcd"))) as ICallable;
      ICallable w = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&who-rcd"))) as ICallable;
      ICallable m = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&message-rcd"))) as ICallable;
      ICallable i = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&irritants-rcd"))) as ICallable;

      if (who is bool && !(bool)who)
      {
        R6RS.Exceptions.RaiseContinueable(
         R6RS.Conditions.Condition(a.Call(), m.Call(message), i.Call(WriteFormat(VectorToList(irritants)))));
      }
      else
      {
        R6RS.Exceptions.RaiseContinueable(
         R6RS.Conditions.Condition(a.Call(), w.Call(who), m.Call(message), i.Call(WriteFormat(VectorToList(irritants)))));
      }

      return Unspecified;
    }
#endif

    [Builtin("error")]
    public static object Error(object who, object message, params object[] irritants)
    {
#if R6RS
      ICallable e = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&error-rcd"))) as ICallable;
      ICallable w = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&who-rcd"))) as ICallable;
      ICallable m = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&message-rcd"))) as ICallable;
      ICallable i = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&irritants-rcd"))) as ICallable;

      if (who is bool && !(bool)who)
      {
        R6RS.Exceptions.RaiseContinueable(
         R6RS.Conditions.Condition(e.Call(), m.Call(message), i.Call(WriteFormat(VectorToList(irritants)))));
      }
      else
      {
        R6RS.Exceptions.RaiseContinueable(
         R6RS.Conditions.Condition(e.Call(), w.Call(who), m.Call(message), i.Call(WriteFormat(VectorToList(irritants)))));
      }

      return Unspecified;
#else
      throw new SchemeException(DisplayFormat(who), DisplayFormat(message), Array.ConvertAll<object, string>(irritants, WriteFormat));
#endif
    }


    [Builtin("eval-string")]
    public static object EvalString(CodeContext cc, string expr)
    {
      SourceUnit su = SourceUnit.CreateSnippet(ScriptEngine, expr);
      Stopwatch sw = Stopwatch.StartNew();
      ScriptCode sc = cc.LanguageContext.CompileSourceCode(su);
      Trace.WriteLine(sw.ElapsedMilliseconds, "Compile - EvalString");
      sw = Stopwatch.StartNew();
      object result = sc.Run(cc.Scope, cc.ModuleContext, false); // causes issues :(
      Trace.WriteLine(sw.ElapsedMilliseconds, "Run - EvalString");
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
