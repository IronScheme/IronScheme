using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using Microsoft.Scripting.Actions;
using System.Reflection;

namespace IronScheme.Runtime
{
  public sealed class MultipleValues
  {
    readonly object[] values;

    public MultipleValues(params object[] values)
    {
      this.values = values;
    }

    public object[] ToArray()
    {
      foreach (object item in values)
      {
        if (item is MultipleValues)
        {
          return (object[]) Closure.AssertionViolation(false, "cannot pass multiple values", values);
        }
      }
      return values;
    }

    public object this[int index]
    {
      get { return values[index]; }
      set { values[index] = value; }
    }
  }


  public static class OptimizedBuiltins
  {
    internal static object Call(ICallable c, params object[] args)
    {
      if (c is BuiltinMethod)
      {
        return c.Call(args);
      }
      else
      {
        List<object> newargs = new List<object>();
        newargs.Add(Closure.Values);
        newargs.AddRange(args);
        return c.Call(newargs.ToArray());
      }
    }

    internal static CallTarget2 MakeCPS(CallTarget1 prim)
    {
      return delegate(object k, object a1)
      {
        return ((ICallable)k).Call(prim(a1));
      };
    }

    internal static CallTarget3 MakeCPS(CallTarget2 prim)
    {
      return delegate(object k, object a1, object a2)
      {
        return ((ICallable)k).Call(prim(a1,a2));
      };
    }

    //[Builtin("call-with-current-continuation"), Builtin("call/cc")]
    public static object CallWithCurrentContinuation(object k, object fc1)
    {
      ICallable fc = (ICallable)(fc1);
      ICallable e = (ICallable)(k);

      CallTarget2 esc = delegate(object ignore, object arg)
      {
        return e.Call(arg);
      };

      if (fc is BuiltinMethod && fc != Closure.CWCC)
      {
        return e.Call(fc.Call(Closure.Make(null, esc)));
      }
      else
      {
        return fc.Call(k, Closure.Make(null, esc));
      }
    }

    //[Builtin("call-with-values")]
    public static object CallWithValues(object producer, object consumer)
    {
      ICallable pro = (ICallable)producer;
      ICallable con = (ICallable)consumer;

#if CPS
      object r = Call(pro);

      if (r is MultipleValues)
      {
        return Call(con, ((MultipleValues)r).ToArray());
      }

      return Call(con, r);
#else

      object r = pro.Call();

      if (r is MultipleValues)
      {
        return con.Call(((MultipleValues)r).ToArray());
      }

      return con.Call(r);
#endif
    }


  }
}
