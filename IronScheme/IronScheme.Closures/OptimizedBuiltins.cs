using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;

namespace IronScheme.Runtime
{
  public static class OptimizedBuiltins
  {
    //[Builtin("call-with-values")]
    public static object CallWithValues(object producer, object consumer)
    {
      ICallable pro = (ICallable)producer;
      ICallable con = (ICallable)consumer;

      object r = pro.Call();

      if (r is object[])
      {
        return con.Call((object[])r);
      }

      return con.Call(r);
    }


  }

#if FALSE
  sealed class BuiltinMethod : ICallable
  {
    readonly MethodBinder meth;
    readonly MethodGroup methods;
    readonly Dictionary<int, ICallable> cache = new Dictionary<int, ICallable>();

    public MethodBinder Binder
    {
      get { return meth; }
    }

    readonly string name;

    public string Name
    {
      get { return name; }
    }

    public override string ToString()
    {
      return Name;
    }

    public MethodBase[] GetMethodBases()
    {
      return methods.GetMethodBases();
    }

    public BuiltinMethod(string name, MethodGroup mg)
    {
      this.name = name;
      this.methods = mg;
      meth = MethodBinder.MakeBinder(IronScheme.Compiler.BaseHelper.binder, mg.Name, mg.GetMethodBases(), BinderType.Normal);

    }

    bool baked = false;

    public object Call(object[] args)
    {
      if (args == null)
      {
        args = new object[0];
      }

      CodeContext context = Compiler.BaseHelper.cc;

      int nargs = args.Length;

      ICallable c;

      if (cache.TryGetValue(nargs, out c))
      {
        return c.Call(args);
      }

      if (!baked)
      {
        try
        {
          Type[] targs = Array.ConvertAll<object, Type>(args, delegate(object input)
          {
            if (input == null)
            {
              return typeof(object);
            }
            else
            {
              return input.GetType();
            }
          });

          MethodCandidate mc = meth.MakeBindingTarget(CallType.None, targs);
          if (mc != null)
          {
            MethodBase mb = mc.Target.Method;

            bool needContext = NeedContext(mb);


            Type dt = CallTargets.GetTargetType(needContext, nargs, false);
            Delegate d = Delegate.CreateDelegate(dt, mb as MethodInfo, false);
            if (d == null)
            {
              d = Delegate.CreateDelegate(needContext ? typeof(CallTargetWithContextN) : typeof(CallTargetN), mb as MethodInfo, false);
            }

            if (d != null)
            {
              cache[nargs] = c = Closure.Make(context, d);
              return c.Call(args);
            }

          }
        }
        catch
        {
          ;
        }
      }
      // fallback
      baked = true;
      return meth.CallReflected(context, CallType.None, args);
    }

    static bool NeedContext(MethodBase mb)
    {
      foreach (ParameterInfo pi in mb.GetParameters())
      {
        return pi.ParameterType == typeof(CodeContext);
      }
      return false;
    }

    #region ICallable Members

    public object Call()
    {
      ICallable c;
      if (cache.TryGetValue(0, out c))
      {
        return c.Call();
      }
      return Call(new object[0]);
    }

    public object Call(object arg1)
    {
      ICallable c;
      if (cache.TryGetValue(1, out c))
      {
        return c.Call(arg1);
      }
      return Call(new object[] { arg1 });
    }

    public object Call(object arg1, object arg2)
    {
      ICallable c;
      if (cache.TryGetValue(2, out c))
      {
        return c.Call(arg1, arg2);
      }
      return Call(new object[] { arg1, arg2 });
    }

    public object Call(object arg1, object arg2, object arg3)
    {
      ICallable c;
      if (cache.TryGetValue(3, out c))
      {
        return c.Call(arg1, arg2, arg3);
      }
      return Call(new object[] { arg1, arg2, arg3 });
    }

    public object Call(object arg1, object arg2, object arg3, object arg4)
    {
      ICallable c;
      if (cache.TryGetValue(4, out c))
      {
        return c.Call(arg1, arg2, arg3, arg4);
      }
      return Call(new object[] { arg1, arg2, arg3, arg4 });
    }

    public object Call(object arg1, object arg2, object arg3, object arg4, object arg5)
    {
      ICallable c;
      if (cache.TryGetValue(5, out c))
      {
        return c.Call(arg1, arg2, arg3, arg4, arg5);
      }
      return Call(new object[] { arg1, arg2, arg3, arg4, arg5 });
    }


    #endregion
  }

#endif
}
