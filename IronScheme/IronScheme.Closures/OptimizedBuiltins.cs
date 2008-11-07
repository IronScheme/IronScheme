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
#if CPS
    internal static object Call(ICallable c, params object[] args)
    {
      if (c is BuiltinMethod && c != Closure.CWCC && c != Closure.CallWithValues)
      {
        return c.Call(args);
      }
      else
      {
        List<object> newargs = new List<object>();
        newargs.Add(Closure.IdentityForCPS);
        newargs.AddRange(args);
        return c.Call(newargs.ToArray());
      }
    }

    internal static object CallWithK(ICallable c, ICallable K, params object[] args)
    {
      if (c is BuiltinMethod && c != Closure.CWCC && c != Closure.CallWithValues)
      {
        return K.Call(c.Call(args));
      }
      else
      {
        List<object> newargs = new List<object>();
        newargs.Add(K);
        newargs.AddRange(args);
        return c.Call(newargs.ToArray());
      }
    }

    internal static CallTarget1 MakeCPS(CallTarget0 prim)
    {
      return delegate(object k)
      {
        return ((ICallable)k).Call(prim());
      };
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

    public static object MakeCPSCallable(object prim)
    {
      CallTarget2 cps = delegate(object k, object args)
      {
        object[] aargs = Closure.ArrayFromCons(args);
        return CallWithK(prim as ICallable, k as ICallable, aargs);
      };
      return Closure.MakeVarArgX(null, cps, 2);
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

      if (fc is BuiltinMethod && fc != Closure.CWCC && fc != Closure.CallWithValues)
      {
        return e.Call(fc.Call(Closure.Make(null, esc)));
      }
      else
      {
        return fc.Call(e, Closure.Make(null, esc));
      }
    }

    //last arg must be a list
    public static object Apply(object k, object fn, object args)
    {
      ICallable c = (ICallable)(fn);
      object[] targs = Closure.ArrayFromCons(args);

      List<object> allargs = new List<object>();

      int i = 0;

      for (; i < targs.Length - 1; i++)
      {
        allargs.Add(targs[i]);
      }

      allargs.AddRange(Closure.ArrayFromCons(targs[i]));

      return OptimizedBuiltins.CallWithK(c, k as ICallable, allargs.ToArray());
    }

    public static object Values(object k, object list)
    {
      ICallable K = (ICallable)k;
      object[] args = Closure.ArrayFromCons(list);
      if (args.Length == 1)
      {
        return K.Call(args[0]);
      }
      else
      {
        return K.Call(new MultipleValues(args));
      }
    }

    //[Builtin("call-with-values")]
    public static object CallWithValues(object k, object producer, object consumer)
    {
      ICallable pro = (ICallable)producer;
      ICallable con = (ICallable)consumer;
      ICallable K = (ICallable)k;

      CallTarget1 ct = delegate(object arg)
      {
        if (arg is MultipleValues)
        {
          return CallWithK(con, K, ((MultipleValues)arg).ToArray());
        }
        return CallWithK(con, K, arg);
      };

      return CallWithK(pro, Closure.Make(null, ct));
    }

#else
    //[Builtin("call-with-values")]
    public static object CallWithValues(object producer, object consumer)
    {
      ICallable pro = (ICallable)producer;
      ICallable con = (ICallable)consumer;

      object r = pro.Call();

      if (r is MultipleValues)
      {
        return con.Call(((MultipleValues)r).ToArray());
      }

      return con.Call(r);
    }
#endif


  }
}
