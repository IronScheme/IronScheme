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
using Microsoft.Scripting.Generation;
using Microsoft.Scripting;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Reflection.Emit;
using System.Collections;
using System.Diagnostics;

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
      if (weh == null)
      {
        weh = SymbolValue(SymbolTable.StringToObject("with-exception-handler")) as Callable;
      }

      return weh.Call(handler, thunk);
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
      if (realraise == null)
      {
        realraise = SymbolValue(SymbolTable.StringToObject("raise")) as Callable;
      }

      return realraise.Call(obj);
    }

    static Callable realraisec;

    [Builtin("raise-continuable")]
    [Obsolete("Implemented in Scheme")]
    public static object RaiseContinueable(object obj)
    {
      if (realraisec == null)
      {
        realraisec = SymbolValue(SymbolTable.StringToObject("raise-continuable")) as Callable;
      }

      return realraisec.Call(obj);
    }

#endif
  }


}

