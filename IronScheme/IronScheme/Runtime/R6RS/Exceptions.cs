#region License
/* Copyright (c) 2007-2013 Llewellyn Pritchard 
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


    public static object GetStackTrace()
    {
      var sf = new StackTrace(2, true);
      List<string> newst = new List<string>();
      var sfs = sf.ToString().Split(Environment.NewLine.ToCharArray(), StringSplitOptions.RemoveEmptyEntries);

      for (int i = 0; i < sfs.Length; i++)
      {
        if (sfs[i].StartsWith(@"   at Microsoft.Scripting.Hosting.ScriptEngine.ExecuteCommand(String code, IScriptModule module)") ||
          sfs[i].StartsWith("   at Microsoft.Scripting.Hosting.ScriptEngine.ExecuteCommand(System.String code, IScriptModule module)"))
        {
          break;
        }
        newst.Add(sfs[i].
          Replace("   at #.", string.Empty).
          Replace("   at ", string.Empty).
          Replace("Microsoft.Scripting.CodeContext $context, ", string.Empty).
          Replace("Microsoft.Scripting.CodeContext $context", string.Empty).
          Replace("CodeContext $context, ", string.Empty).
          Replace("CodeContext $context", string.Empty).
          Replace("System.Object ", string.Empty).
          Replace("Object ", string.Empty));
      }

      return newst.ToArray();
    }
  }
}

