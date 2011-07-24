#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.IO;
using System.Text;
using Microsoft.Scripting;

namespace IronScheme.Runtime
{

  public partial class Builtins
  {
    [Builtin]
    public static object UnGenSym(object symbol)
    {
      var sym = RequiresNotNull<SymbolId>(symbol);

      string ss = SymbolTable.IdToString(sym);
      //name is between 1st and 2nd $
      int start = ss.IndexOf('$') + 1;
      if (start > 0)
      {
        int count = ss.IndexOf('$', start) - start;

        if (count > 0)
        {
          ss = ss.Substring(start, count);
          return SymbolTable.StringToObject(ss);
        }
      }
      return symbol;
    }

    internal static SymbolId UnGenSymInternal(SymbolId sym)
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

    internal static object CleanWho(object who)
    {
      string name = who.ToString();
      int i = name.LastIndexOf('$');
      if (i < 0)
      {
        return who;
      }
      return SymbolTable.StringToObject(name.Substring(0, i));
    }
#if !USE_GLUE
    [Obsolete("Remove when possible")]
    internal static object IODecodingError()
    {
      if (IsR6RSLoaded())
      {
        Callable u = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&i/o-decoding-rcd"))) as Callable;
#if CPS
        throw (Exception)R6RS.Conditions.Condition(OptimizedBuiltins.Call(u,FALSE));
#else
        return R6RS.Exceptions.Raise(
          R6RS.Conditions.Condition(u.Call(FALSE)));
#endif
      }
      else
      {
        throw new Exception("i/o decoding");
      }
    }

    [Obsolete("Remove when possible")]
    internal static object IOEncodingError()
    {
      if (IsR6RSLoaded())
      {
        Callable u = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&i/o-encoding-rcd"))) as Callable;

#if CPS
        throw (Exception)R6RS.Conditions.Condition(OptimizedBuiltins.Call(u, FALSE, FALSE));
#else
        return R6RS.Exceptions.Raise(
          R6RS.Conditions.Condition(u.Call(FALSE, FALSE)));
#endif
      }
      else
      {
        throw new Exception("i/o encoding");
      }
    }

    [Obsolete("Remove when possible")]
    internal static object UndefinedError(object sym)
    {
      if (IsR6RSLoaded())
      {
        Callable u = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&undefined-rcd"))) as Callable;
        Callable m = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&message-rcd"))) as Callable;
        Callable i = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&irritants-rcd"))) as Callable;

        sym = UnGenSym(sym);

#if CPS
        throw
          (Exception)R6RS.Conditions.Condition(
            OptimizedBuiltins.Call(u), 
            OptimizedBuiltins.Call(m, "attempted to use undefined symbol"), 
            OptimizedBuiltins.Call(i, List(sym)));

#else

        return R6RS.Exceptions.Raise(
          R6RS.Conditions.Condition(u.Call(), m.Call("attempted to use undefined symbol"), i.Call(List(sym))));
#endif
      }
      else
      {
        throw new Exception("undefined symbol: " + UnGenSymInternal(RequiresNotNull<SymbolId>(sym)));
      }
    }

    [Obsolete("Remove when possible")]
    internal static object LexicalError(string msg, object what)
    {
      if (IsR6RSLoaded())
      {
        Callable l = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&lexical-rcd"))) as Callable;
        Callable m = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&message-rcd"))) as Callable;
        Callable i = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&irritants-rcd"))) as Callable;

#if CPS
        throw (Exception)R6RS.Conditions.Condition(OptimizedBuiltins.Call(l), 
          OptimizedBuiltins.Call(m,msg), 
          OptimizedBuiltins.Call(i, List(what)));
#else
        return R6RS.Exceptions.Raise(
          R6RS.Conditions.Condition(l.Call(), m.Call(msg), i.Call(List(what))));
#endif
      }
      else
      {
        throw new Exception(string.Format("lexical error: {0} ({1})", msg, what));
      }

    }

    [Obsolete("Remove when possible")]
    internal static object SyntaxError(object who, object message, object form, object subform)
    {
      if (IsR6RSLoaded())
      {
        Callable s = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&syntax-rcd"))) as Callable;
        Callable w = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&who-rcd"))) as Callable;
        Callable m = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&message-rcd"))) as Callable;

        if (!IsTrue(who))
        {
          who = GetCaller();
        }
#if CPS
        throw
  (Exception)R6RS.Conditions.Condition(OptimizedBuiltins.Call(w, CleanWho(who)), OptimizedBuiltins.Call(m, message), OptimizedBuiltins.Call(s, form, subform));

#else
        return R6RS.Exceptions.Raise(
          R6RS.Conditions.Condition(w.Call(CleanWho(who)), m.Call(message), s.Call(form, subform)));
#endif
      }
      else
      {
        throw new Exception(string.Format("syntax error: {0} ({1})", message, form));
      }
    }

    [Obsolete("Remove when possible")]
    internal static object FileNotFoundViolation(object who, object message, object filename)
    {
      if (IsR6RSLoaded())
      {
        Callable a = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&assertion-rcd"))) as Callable;
        Callable w = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&who-rcd"))) as Callable;
        Callable m = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&message-rcd"))) as Callable;
        Callable i = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&i/o-file-does-not-exist-rcd"))) as Callable;

        if (!IsTrue(who))
        {
          who = GetCaller();
        }
#if CPS
        throw
          (Exception)R6RS.Conditions.Condition(
            OptimizedBuiltins.Call(a), 
            OptimizedBuiltins.Call(w, CleanWho(who)), 
            OptimizedBuiltins.Call(m, message), 
            OptimizedBuiltins.Call(i,filename));

#else
        return R6RS.Exceptions.Raise(
         R6RS.Conditions.Condition(a.Call(), w.Call(CleanWho(who)), m.Call(message), i.Call(filename)));
#endif
      }
      else
      {
        throw new FileNotFoundException(filename as string);
      }
    }

    [Obsolete("Remove when possible")]
    internal static object IOPortViolation(object who, object message, object port)
    {
      if (IsR6RSLoaded())
      {
        Callable a = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&assertion-rcd"))) as Callable;
        Callable w = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&who-rcd"))) as Callable;
        Callable m = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&message-rcd"))) as Callable;
        Callable i = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&i/o-port-rcd"))) as Callable;

        if (!IsTrue(who))
        {
          who = GetCaller();
        }
#if CPS
        throw
           (Exception)R6RS.Conditions.Condition(
           OptimizedBuiltins.Call(a),
           OptimizedBuiltins.Call(w,CleanWho(who)),
           OptimizedBuiltins.Call(m,message),
           OptimizedBuiltins.Call(i,port));
#else
        return R6RS.Exceptions.Raise(
           R6RS.Conditions.Condition(
           a.Call(), 
           w.Call(CleanWho(who)), 
           m.Call(message), 
           i.Call(port)));
#endif
      }
      else
      {
        throw new IOException(message as string);
      }
    }

    [Obsolete("Remove when possible")]
    internal static object FileAlreadyExistsViolation(object who, object filename)
    {
      if (IsR6RSLoaded())
      {
        Callable a = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&i/o-file-already-exists-rcd"))) as Callable;
        Callable w = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&who-rcd"))) as Callable;

        if (!IsTrue(who))
        {
          who = GetCaller();
        }
#if CPS
        throw (Exception)R6RS.Conditions.Condition(
          OptimizedBuiltins.Call(a,filename),
          OptimizedBuiltins.Call(w,CleanWho(who)));
#else
        return R6RS.Exceptions.Raise(
         R6RS.Conditions.Condition(a.Call(filename), w.Call(CleanWho(who))));
#endif
      }
      else
      {
        throw new IOException(filename as string);
      }
    }

    [Obsolete("Remove when possible")]
    internal static object FileInUseViolation(object who, object filename)
    {
      if (IsR6RSLoaded())
      {
        Callable a = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&i/o-file-protection-rcd"))) as Callable;
        Callable w = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&who-rcd"))) as Callable;

        if (!IsTrue(who))
        {
          who = GetCaller();
        }
#if CPS
        throw
          (Exception)R6RS.Conditions.Condition(
            OptimizedBuiltins.Call(a,filename),
            OptimizedBuiltins.Call(w, CleanWho(who)));
#else
        return R6RS.Exceptions.Raise(
         R6RS.Conditions.Condition(a.Call(filename), w.Call(CleanWho(who))));
#endif
      }
      else
      {
        throw new IOException(filename as string);
      }
    }
#endif

    static bool r6rsloaded = false;

    protected static bool IsR6RSLoaded()
    {
      return r6rsloaded || (r6rsloaded = Context.Scope.ModuleScope.ContainsName(SymbolTable.StringToId("r6rs-loaded")));
    }

#if !USE_GLUE
    [Obsolete("Remove when possible")]
    internal static object AssertionViolation(object who, object message, object irritant1)
    {
      return AssertionViolation(who, message, new object[] { irritant1 });
    }

    [Obsolete("Remove when possible")]
    internal static object AssertionViolation(object who, object message, object irritant1, object irritant2)
    {
      return AssertionViolation(who, message, new object[] { irritant1, irritant2 });
    }

    [Obsolete("Remove when possible")]
    internal static object AssertionViolation(object who, object message, params object[] irritants)
    {
      if (IsR6RSLoaded())
      {

#if CPS
        Callable a = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&assertion-rcd"))) as Callable;
        Callable w = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&who-rcd"))) as Callable;
        Callable m = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&message-rcd"))) as Callable;
        Callable i = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&irritants-rcd"))) as Callable;

        if (IsTrue(who))
        {
          throw
             (Exception)R6RS.Conditions.Condition(OptimizedBuiltins.Call(a), OptimizedBuiltins.Call(w, CleanWho(who)), OptimizedBuiltins.Call(m, message), OptimizedBuiltins.Call(i, VectorToList(irritants)));
        }
        else
        {
          throw
             (Exception)R6RS.Conditions.Condition(OptimizedBuiltins.Call(a), OptimizedBuiltins.Call(m, message), OptimizedBuiltins.Call(i, VectorToList(irritants)));
        }
#else
        Callable a = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&assertion-rcd"))) as Callable;
        Callable w = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&who-rcd"))) as Callable;
        Callable m = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&message-rcd"))) as Callable;
        Callable i = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&irritants-rcd"))) as Callable;

        if (IsTrue(who))
        {
          return R6RS.Exceptions.Raise(
             R6RS.Conditions.Condition(a.Call(), w.Call(CleanWho(who)), m.Call(message), i.Call(VectorToList(irritants))));
        }
        else
        {
          Callable wh = R6RS.Records.RecordConstructor(SymbolValue(SymbolTable.StringToObject("&where-rcd"))) as Callable;
          return R6RS.Exceptions.Raise(
             R6RS.Conditions.Condition(a.Call(), wh.Call(GetCaller()), m.Call(message), i.Call(VectorToList(irritants))));
        }
#endif
      }
      else
      {
        throw new Exception(string.Format("assertion-violation: {0}", message));
      }
    }

#endif

    static readonly int TICKS = (int)((DateTime.Now.Ticks >> 16) & 0x7FFFFFFF);

    static int anonsymcount = 0;
    static int symcount = 0;

    static char[] symchars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ@=".ToCharArray(); //64 for ease

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
      return SymbolTable.StringToObject("g$" + anonsymcount++ + "$" + TICKSTRING);
    }

    [Builtin]
    public static object GenSym(object name)
    {
      if (name is string)
      {
        string s = RequiresNotNull<string>(name);
        return SymbolTable.StringToObject("g$" + s + "$" + symcount++ + "$" + TICKSTRING);
      }
      else
      {
        SymbolId s = UnGenSymInternal(RequiresNotNull<SymbolId>(name));
        return SymbolTable.StringToObject("g$" + s + "$" + symcount++ + "$" + TICKSTRING);
      }
    }
  }
}
