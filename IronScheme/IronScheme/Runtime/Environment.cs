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

  public partial class Builtins
  {
    [Builtin]
    public object UnGenSym(object symbol)
    {
      return UnGenSym(RequiresNotNull<SymbolId>(symbol));
    }

    internal static SymbolId UnGenSym(SymbolId sym)
    {
      string ss = SymbolTable.IdToString(sym);
      //name is between 1st and 2nd $
      int start = ss.IndexOf('$') + 1;
      if (start > 0)
      {
        int count = ss.IndexOf('$', start) - start;

        if (count > 0)
        {
          ss = ss.Substring(start, count);
          return SymbolTable.StringToId(ss);
        }
      }
      return sym;
    }

    public static object UndefinedError(object sym)
    {
      if (IsR6RSLoaded())
      {
        ICallable u = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&undefined-rcd"))) as ICallable;
        ICallable m = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&message-rcd"))) as ICallable;
        ICallable i = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&irritants-rcd"))) as ICallable;

        sym = UnGenSym(RequiresNotNull<SymbolId>(sym));

        return R6RS.Exceptions.RaiseContinueable(
          R6RS.Conditions.Condition(u.Call(), m.Call("attempted to use undefined symbol"), i.Call(List(sym))));
      }
      else
      {
        throw new Exception("undefined symbol: " + UnGenSym(RequiresNotNull<SymbolId>(sym)));
      }
    }

    public static object LexicalError(string msg, object what)
    {
      if (IsR6RSLoaded())
      {
        ICallable l = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&lexical-rcd"))) as ICallable;
        ICallable m = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&message-rcd"))) as ICallable;
        ICallable i = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&irritants-rcd"))) as ICallable;

        return R6RS.Exceptions.RaiseContinueable(
          R6RS.Conditions.Condition(l.Call(), m.Call(msg), i.Call(List(what))));
      }
      else
      {
        throw new Exception(string.Format("lexical error: {0} ({1})", msg, what));
      }

    }

    public static object SyntaxError(object who, object message, object form, object subform)
    {
      if (IsR6RSLoaded())
      {
        ICallable s = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&syntax-rcd"))) as ICallable;
        ICallable w = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&who-rcd"))) as ICallable;
        ICallable m = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&message-rcd"))) as ICallable;

        if (who is bool && !(bool)who)
        {
          return R6RS.Exceptions.RaiseContinueable(
            R6RS.Conditions.Condition(m.Call(message), s.Call(form, subform)));
        }
        else
        {
          return R6RS.Exceptions.RaiseContinueable(
            R6RS.Conditions.Condition(w.Call(who), m.Call(message), s.Call(form, subform)));
        }
      }
      else
      {
        throw new Exception(string.Format("syntax error: {0} ({1})", message, form));
      }
    }

    public static object FileNotFoundViolation(object who, object message, object filename)
    {
      if (IsR6RSLoaded())
      {
        ICallable a = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&assertion-rcd"))) as ICallable;
        ICallable w = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&who-rcd"))) as ICallable;
        ICallable m = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&message-rcd"))) as ICallable;
        ICallable i = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&i/o-file-does-not-exist-rcd"))) as ICallable;

        if (who is bool && !(bool)who)
        {
          return R6RS.Exceptions.RaiseContinueable(
           R6RS.Conditions.Condition(a.Call(), m.Call(message), i.Call(filename)));
        }
        else
        {
          return R6RS.Exceptions.RaiseContinueable(
           R6RS.Conditions.Condition(a.Call(), w.Call(who), m.Call(message), i.Call(filename)));
        }
      }
      else
      {
        throw new FileNotFoundException(filename as string);
      }
    }

    public static object IOPortViolation(object who, object message, object port)
    {
      if (IsR6RSLoaded())
      {
        ICallable a = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&assertion-rcd"))) as ICallable;
        ICallable w = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&who-rcd"))) as ICallable;
        ICallable m = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&message-rcd"))) as ICallable;
        ICallable i = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&i/o-port-rcd"))) as ICallable;

        if (who is bool && !(bool)who)
        {
          return R6RS.Exceptions.RaiseContinueable(
           R6RS.Conditions.Condition(a.Call(), m.Call(message), i.Call(port)));
        }
        else
        {
          return R6RS.Exceptions.RaiseContinueable(
           R6RS.Conditions.Condition(a.Call(), w.Call(who), m.Call(message), i.Call(port)));
        }
      }
      else
      {
        throw new IOException(message as string);
      }
    }

    static bool IsR6RSLoaded()
    {
      return Context.Scope.ModuleScope.ContainsName(SymbolTable.StringToId("&assertion-rcd"));
    }

    [Builtin("assertion-violation")]
    public static object AssertionViolation(object who, object message, object irritant1)
    {
      return AssertionViolation(who, message, new object[] { irritant1 });
    }

    [Builtin("assertion-violation")]
    public static object AssertionViolation(object who, object message, object irritant1, object irritant2)
    {
      return AssertionViolation(who, message, new object[] { irritant1, irritant2 });
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
          return R6RS.Exceptions.RaiseContinueable(
           R6RS.Conditions.Condition(a.Call(), m.Call(message), i.Call(VectorToList(irritants))));
        }
        else
        {
          return R6RS.Exceptions.RaiseContinueable(
           R6RS.Conditions.Condition(a.Call(), w.Call(who), m.Call(message), i.Call(VectorToList(irritants))));
        }

      }
      else
      {
        throw new Exception(string.Format("assertion-violation: {0}", message));
      }
    }

    [Builtin("error")]
    public static object Error(object who, object message, params object[] irritants)
    {
      if (IsR6RSLoaded())
      {
        ICallable e = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&error-rcd"))) as ICallable;
        ICallable w = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&who-rcd"))) as ICallable;
        ICallable m = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&message-rcd"))) as ICallable;
        ICallable i = R6RS.Records.RecordConstructor(SymbolValue(Context, SymbolTable.StringToId("&irritants-rcd"))) as ICallable;

        if (who is bool && !(bool)who)
        {
          return R6RS.Exceptions.RaiseContinueable(
           R6RS.Conditions.Condition(e.Call(), m.Call(message), i.Call(VectorToList(irritants))));
        }
        else
        {
          return R6RS.Exceptions.RaiseContinueable(
           R6RS.Conditions.Condition(e.Call(), w.Call(who), m.Call(message), i.Call(VectorToList(irritants))));
        }
      }
      else
      {
        throw new Exception(string.Format("error: {0}", message));
      }
    }

    static readonly int TICKS = (int)((DateTime.Now.Ticks >> 16) & 0x7FFFFFFF);

    static int anonsymcount = 0;
    static int symcount = 0;

    static char[] symchars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ@&".ToCharArray(); //64 for ease

    static string PackInt(int i)
    {
      StringBuilder sb = new StringBuilder();
      do
      {
        int r = i % 64;
        sb.Append(symchars[r]);

        i /= 64;
      }
      while (i > 0) ;

      // need to reverse, not really needed, but lets be correct for now
      char[] res = new char[sb.Length];

      for (int j = 0; j < res.Length; j++)
      {
        res[res.Length - j - 1] = sb[j];
      }

      return new string(res);
    }

    static readonly string TICKSTRING = PackInt(TICKS);

    [Builtin]
    public static object GenSym()
    {
      return SymbolTable.StringToId("g$" + anonsymcount++ + "$" + TICKSTRING);
    }

    [Builtin]
    public static object GenSym(object name)
    {
      if (name is string)
      {
        string s = RequiresNotNull<string>(name);
        return SymbolTable.StringToId("g$" + s + "$" + symcount++ + "$" + TICKSTRING);
      }
      else
      {
        SymbolId s = UnGenSym(RequiresNotNull<SymbolId>(name));
        return SymbolTable.StringToId("g$" + s + "$" + symcount++ + "$" + TICKSTRING);
      }
    }
  }
}
