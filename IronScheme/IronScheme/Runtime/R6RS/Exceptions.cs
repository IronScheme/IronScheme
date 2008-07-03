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

namespace IronScheme.Runtime.R6RS
{
  public class Exceptions : Builtins
  {
    static Dictionary<Exception, bool> continuablemap = new Dictionary<Exception, bool>();

    static Stack<ICallable> handlerstack = new Stack<ICallable>();
    static ICallable defaulthandler;

    static ICallable CurrentHandler
    {
      get
      {
        if (handlerstack.Count > 0)
        {
          return handlerstack.Peek();
        }
        return null;
      }
    }

    //(with-exception-handler handler thunk)
    [Builtin("with-exception-handler")]
    public static object WithExceptionHandler(object handler, object thunk)
    {
      ICallable h = RequiresNotNull<ICallable>(handler);
      ICallable t = RequiresNotNull<ICallable>(thunk);

      InitDefaultHandler();

      handlerstack.Push(h);

      try
      {
        return t.Call();
      }
      finally
      {
        handlerstack.Pop();
      }
    }


    static void InitDefaultHandler()
    {
      if (defaulthandler == null)
      {
        defaulthandler = SymbolValue(cc, SymbolTable.StringToId("default-exception-handler")) as ICallable;
        handlerstack.Push(defaulthandler);
      }
    }

    [Builtin("raise")]
    public static object Raise(object obj)
    {
      InitDefaultHandler();

      ICallable ch = CurrentHandler;
      if (ch != null)
      {
        try
        {
          handlerstack.Pop();
          return ch.Call(obj);
        }
        finally
        {
          handlerstack.Push(ch);
        }
      }

      Exception ex = RequiresNotNull<Exception>(obj);
      continuablemap[ex] = false;
      
      throw ex;
    }

    // erk??
    [Builtin("raise-continuable")]
    public static object RaiseContinueable(object obj)
    {
      InitDefaultHandler();

      ICallable ch = CurrentHandler;
      if (ch != null)
      {
        try
        {
          handlerstack.Pop();
          return ch.Call(obj);
        }
        finally
        {
          handlerstack.Push(ch);
        }
      }

      Exception ex = RequiresNotNull<Exception>(obj);
      continuablemap[ex] = true;
      throw ex;
    }
  }
}

