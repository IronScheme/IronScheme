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
    
    [Builtin("call-with-current-continuation"), Builtin("call/cc")]
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

    //procedure:  (apply proc arg1 ... args) 
    //Proc must be a procedure and args must be a list. Calls proc with the elements of the list (append (list arg1 ...) args) as the actual arguments.
    
    [Builtin]
    public static object Apply(CodeContext cc, object fn, params object[] args)
    {
      object[] head = ArrayUtils.RemoveLast(args);
      Cons last = Requires<Runtime.Cons>(args[args.Length - 1]);

      return Apply(cc, fn, Append(List(head), last));
    }

    [Builtin]
    public static object Apply(CodeContext cc, object fn, object list)
    {
      Cons args = Requires<Runtime.Cons>(list);
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
    public static Cons Map(CodeContext cc, object fn, params object[] lists)
    {
      if (lists == null)
      {
        return null;
      }
      ArrayList returns = new ArrayList();
      foreach (Cons obj in new MultiEnumerable(lists))
      {
        returns.Add(Apply(cc, fn, obj));
      }
      return Runtime.Cons.FromList(returns);
    }


    [Builtin("for-each")]
    public static object ForEach(CodeContext cc, object fn, object list)
    {
      Cons c = Requires<Runtime.Cons>(list);

      while (c != null)
      {
        Apply(cc, fn, c);
        c = c.Cdr as Cons;
      }
      return Unspecified;
    }

    [Builtin("for-each")]
    public static object ForEach(CodeContext cc, object fn, params object[] lists)
    {
      foreach (Cons obj in new MultiEnumerable(lists))
      {
        Apply(cc, fn, obj);
      }
      return Unspecified;
    }

  }
}
