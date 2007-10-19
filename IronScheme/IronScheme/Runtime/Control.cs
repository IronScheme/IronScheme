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
using Microsoft.Scripting.Math;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Collections;
using Microsoft.Scripting;

namespace IronScheme.Runtime
{
  public static partial class Builtins
  {
    [Builtin]
    public static object Values(params object[] values)
    {
      throw new NotImplementedException();
    }

    [Builtin("call-with-values")]
    public static object CallWithValues(object producer, object consumer)
    {
      throw new NotImplementedException();
    }

    [Builtin("dynamic-wind")]
    public static object DynamicWind(CodeContext cc, object infunc, object bodyfunc, object outfunc)
    {
      FastCallable inf = RequiresNotNull<FastCallable>(infunc);
      FastCallable bodyf = RequiresNotNull<FastCallable>(bodyfunc);
      FastCallable outf = RequiresNotNull<FastCallable>(outfunc);

      inf.Call(cc);

      try
      {
        return bodyf.Call(cc);
      }
      finally
      {
        outf.Call(cc);
      }
    }

    class Continuation : Exception
    {
      readonly object value;

      public object Value
      {
        get { return this.value; }
      } 

      public Continuation(object value)
      {
        this.value = value;
      }
    }

    static object InvokeContinuation(object value)
    {
      throw new Continuation(value);
    }
    
    [Builtin("call-with-current-continuation")]
    public static object CallWithCurrentContinuation(CodeContext cc, object fc1)
    {
      FastCallable fc = RequiresNotNull<FastCallable>(fc1);
      try
      {
        CallTarget1 exitproc = InvokeContinuation;
        FastCallable fce = Closure.Make(cc, exitproc, "invoke-continuation");
        return fc.Call(cc, fce);
      }
      catch (Continuation c)
      {
        return c.Value;
      }
    }

    [Builtin]
    public static object Force(object promise)
    {
      Promise p = RequiresNotNull<Promise>(promise);
      return p.Force();
    }

    [Builtin("procedure?")]
    public static bool IsProcedure(object obj)
    {
      return obj is FastCallable; 
    }

    [Builtin]
    public static object Apply(CodeContext cc, object fn)
    {
      if (fn is FastCallable)
      {
        return ((FastCallable)fn).Call(cc);
      }
      else if (fn is Delegate)
      {
        Delegate d = (Delegate) fn;
        return Apply(cc, Closure.Make(cc, d, d.Method.Name));
      }
      else
      {
        throw new ArgumentTypeException("fn must be FastCallable or a delegate");
      }
    }

    static object ApplyInternal(CodeContext cc, FastCallable fn, Cons args)
    {
      List<object> targs = new List<object>();
      while (args != null)
      {
        targs.Add(args.Car);
        args = args.Cdr as Cons;
      }
      return fn.Call(cc, targs.ToArray());
    }

    [Builtin]
    public static object Apply(CodeContext cc, object fn, params Cons[] lists)
    {
      ArrayList args = new ArrayList();
      //if (lists == null)
      //{
      //  args.Add(null);
      //}
      //else
      //{
      //  for (int i = 0; i < lists.Length - 1; i++)
      //  {
      //    args.Add(lists[i]);
      //  }
      //  if (lists.Length > 0)
      //  {
      //    foreach (object var in lists[lists.Length - 1] as IEnumerable)
      //    {
      //      args.Add(var);
      //    }
      //  }
      //}
      return null;

      //return ApplyInternal(cc, fn, args);
    }

    [Builtin]
    public static object Apply(CodeContext cc, object fn, Cons args)
    {
      if (fn is FastCallable)
      {
        return ApplyInternal(cc, fn as FastCallable, args);
      }
      else if (fn is Delegate)
      {
        Delegate d = (Delegate)fn;
        return ApplyInternal(cc, Closure.Make(cc, d, d.Method.Name), args);
      }
      else
      {
        throw new ArgumentTypeException("fn must be FastCallable or a delegate");
      }
    }

    [Builtin]
    public static Cons Map(CodeContext cc, object fn, object lst)
    {
      Cons list = Requires<Runtime.Cons>(lst);
      ArrayList returns = new ArrayList();
      while (list != null)
      {
        returns.Add(Apply(cc, fn, new Cons(list.Car)));
        list = list.Cdr as Cons;
      }
      return Runtime.Cons.FromList(returns);
    }

    [Builtin]
    public static Cons Map(CodeContext cc, object fn, params Cons[] lists)
    {
      if (lists == null)
      {
        return null;
      }
      ArrayList returns = new ArrayList();
      //foreach (Cons obj in new MultiEnumerable(lists))
      //{
      //  returns.Add(Apply(cc, fn, obj));
      //}
      return Runtime.Cons.FromList(returns);
    }


    [Builtin("for-each")]
    public static void ForEach(CodeContext cc, object fn, Cons list)
    {
      ArrayList returns = new ArrayList();
      //foreach (object obj in list)
      //{
      //  returns.Add(Apply(cc, fn, (IEnumerable) new object[] { obj }));
      //}
    }

    [Builtin("for-each")]
    public static void ForEach(CodeContext cc, object fn, params Cons[] lists)
    {
      ArrayList returns = new ArrayList();
      //foreach (Cons obj in new MultiEnumerable(lists))
      //{
      //  returns.Add(Apply(cc, fn, obj));
      //}
    }

  }
}
