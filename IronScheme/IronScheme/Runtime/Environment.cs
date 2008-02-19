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
using System.Text.RegularExpressions;

namespace IronScheme.Runtime
{

  public partial class Builtins
  {
    [Builtin("r6rs-mode?")]
    public static object IsR6RSMode()
    {
      return TRUE;
    }

    static SymbolId UnGenSym(SymbolId sym)
    {
      string ss = SymbolTable.IdToString(sym);
      Match m = Regex.Match(ss, @".*g\$(?<id>.*?)\$\d+.*");
      if (m.Success)
      {
        return SymbolTable.StringToId(m.Groups["id"].Value);
      }
      return sym;
    }

    public static object UndefinedError(object sym)
    {
      ICallable u = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&undefined-rcd"))) as ICallable;
      ICallable i = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&irritants-rcd"))) as ICallable;

      sym = UnGenSym(RequiresNotNull<SymbolId>(sym));

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
          R6RS.Conditions.Condition(m.Call(message), s.Call(form, subform)));
      }
      else
      {
        R6RS.Exceptions.RaiseContinueable(
          R6RS.Conditions.Condition(w.Call(who), m.Call(message), s.Call(form, subform)));
      }

      return Unspecified;
    }

    public static object FileNotFoundViolation(object who, object message, object filename)
    {
      ICallable a = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&assertion-rcd"))) as ICallable;
      ICallable w = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&who-rcd"))) as ICallable;
      ICallable m = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&message-rcd"))) as ICallable;
      ICallable i = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&i/o-file-does-not-exist-rcd"))) as ICallable;

      if (who is bool && !(bool)who)
      {
        R6RS.Exceptions.RaiseContinueable(
         R6RS.Conditions.Condition(a.Call(), m.Call(message), i.Call(filename)));
      }
      else
      {
        R6RS.Exceptions.RaiseContinueable(
         R6RS.Conditions.Condition(a.Call(), w.Call(who), m.Call(message), i.Call(filename)));
      }

      return Unspecified;
    }

    static bool IsR6RSLoaded()
    {
      return Context.Scope.ModuleScope.ContainsName(SymbolTable.StringToId("&assertion-rcd"));
    }


    [Builtin("assertion-violation")]
    public static object AssertionViolation(object who, object message, params object[] irritants)
    {
      if (IsR6RSLoaded())
      {
        ICallable a = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&assertion-rcd"))) as ICallable;
        ICallable w = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&who-rcd"))) as ICallable;
        ICallable m = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&message-rcd"))) as ICallable;
        ICallable i = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&irritants-rcd"))) as ICallable;

        if (who is bool && !(bool)who)
        {
          R6RS.Exceptions.RaiseContinueable(
           R6RS.Conditions.Condition(a.Call(), m.Call(message), i.Call(VectorToList(irritants))));
        }
        else
        {
          R6RS.Exceptions.RaiseContinueable(
           R6RS.Conditions.Condition(a.Call(), w.Call(who), m.Call(message), i.Call(VectorToList(irritants))));
        }


        return Unspecified;
      }
      else
      {
        throw new Exception(string.Format("{0} - {1}", who, message));
      }
    
    }

    [Builtin("error")]
    public static object Error(object who, object message, params object[] irritants)
    {
      ICallable e = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&error-rcd"))) as ICallable;
      ICallable w = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&who-rcd"))) as ICallable;
      ICallable m = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&message-rcd"))) as ICallable;
      ICallable i = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&irritants-rcd"))) as ICallable;

      if (who is bool && !(bool)who)
      {
        R6RS.Exceptions.RaiseContinueable(
         R6RS.Conditions.Condition(e.Call(), m.Call(message), i.Call(VectorToList(irritants))));
      }
      else
      {
        R6RS.Exceptions.RaiseContinueable(
         R6RS.Conditions.Condition(e.Call(), w.Call(who), m.Call(message), i.Call(VectorToList(irritants))));
      }

      return Unspecified;
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
  }
}
