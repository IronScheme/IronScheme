#region License
/* Copyright (c) 2007,2008,2009,2010 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.Diagnostics;
using Microsoft.Scripting;

namespace IronScheme.Runtime.R6RS
{
  public class Exceptions : Builtins
  {
#if !CPS
    [Builtin("with-clr-exception-handler")]
    public static object WithClrExceptionHandler(object handler, object thunk)
    {
      Callable h = RequiresNotNull<Callable>(handler);
      Callable t = RequiresNotNull<Callable>(thunk);
      try
      {
        return t.Call();
      }
      catch (Continuation)
      {
        throw;
      }
      catch (Exception ex)
      {
        return h.Call(ex);
      }
    }

    static Callable weh;

    //(with-exception-handler handler thunk)
    [Builtin("with-exception-handler")]
    [Obsolete("Implemented in Scheme")]
    public static object WithExceptionHandler(object handler, object thunk)
    {
      if (IsR6RSLoaded())
      {
        if (weh == null)
        {
          weh = SymbolValue(SymbolTable.StringToObject("with-exception-handler")) as Callable;
        }

        return weh.Call(handler, thunk);
      }
      else
      {
        throw new Exception("R6RS not loaded when calling 'with-exception-handler'");
      }
    }

    public static object GetStackTrace()
    {
      var sf = new StackTrace(2, true);
      List<string> newst = new List<string>();
      var sfs = sf.ToString().Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries);

      for (int i = 0; i < sfs.Length; i++)
      {
        if (sfs[i].StartsWith(@"   at Microsoft.Scripting.Hosting.ScriptEngine.ExecuteCommand(String code, IScriptModule module)"))
        {
          break;
        }
        newst.Add(sfs[i].
          Replace("   at #.", string.Empty).
          Replace("   at ", string.Empty).
          Replace("CodeContext $context, ", string.Empty).
          Replace("Object ", string.Empty));
      }

      return newst.ToArray();
    }

    static Callable realraise;

    [Builtin("raise")]
    [Obsolete("Implemented in Scheme")]
    public static object Raise(object obj)
    {
      if (IsR6RSLoaded())
      {
        if (realraise == null)
        {
          realraise = SymbolValue(SymbolTable.StringToObject("raise")) as Callable;
        }

        return realraise.Call(obj);
      }
      else
      {
        throw new Exception("R6RS not loaded when calling 'raise'");
      }
    }

    static Callable realraisec;

    [Builtin("raise-continuable")]
    [Obsolete("Implemented in Scheme")]
    public static object RaiseContinueable(object obj)
    {
      if (IsR6RSLoaded())
      {
        if (realraisec == null)
        {
          realraisec = SymbolValue(SymbolTable.StringToObject("raise-continuable")) as Callable;
        }

        return realraisec.Call(obj);
      }
      else
      {
        throw new Exception("R6RS not loaded when calling 'raise-continuable'");
      }
    }

#endif
  }


}

