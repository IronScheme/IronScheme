using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.Scripting;
using Microsoft.Scripting.Actions;
using System.Reflection;

namespace IronScheme.Runtime
{
  public sealed class BuiltinMethod : ICallable
  {
    readonly MethodBinder meth;
    readonly MethodBase[] methods;
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

    object ICallable.Arity
    {
      get
      {
        List<object> arities = new List<object>();

        foreach (MethodBase m in methods)
        {
          ParameterInfo[] pis = m.GetParameters();
          int pc = pis.Length;
          if (pis.Length > 0 && pis[0].ParameterType == typeof(CodeContext))
          {
            pc--;
          }
          if (pis.Length > 0 && pis[pis.Length - 1].IsDefined(typeof(ParamArrayAttribute), false))
          {
            arities.Add((double)(pc - 1));
          }
          else
          {
            arities.Add(pc);
          }
        }
        if (arities.Count == 1)
        {
          return arities[0];
        }
        else
        {
          return Closure.ConsFromArray(arities.ToArray());
        }
      }
    }

    public override string ToString()
    {
      return Name;
    }

    public MethodBase[] GetMethodBases()
    {
      return methods;
    }

    public static ActionBinder binder;
    public static CodeContext context;

    public BuiltinMethod(string name, MethodBase[] mg)
    {
      this.name = name;
      this.methods = mg;
      meth = MethodBinder.MakeBinder(binder, name, mg, BinderType.Normal);

    }

    bool baked = false;

    public object Call(object[] args)
    {
      if (args == null)
      {
        args = new object[0];
      }

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


            Type dt = nargs > 5 ? typeof(CallTargetN) : CallTargets.GetTargetType(needContext, nargs, false);
            Delegate d = Delegate.CreateDelegate(dt, mb as MethodInfo, false);
            if (d == null)
            {
              d = Delegate.CreateDelegate(needContext ? typeof(CallTargetWithContextN) : typeof(CallTargetN), mb as MethodInfo, false);
            }

            if (d != null)
            {
              cache[nargs] = c = Closure.Make(context, d);
            }
          }
        }
        catch
        {
          ;
        }

        if (c != null)
        {
          return c.Call(args);
        }
      }
      // fallback
      baked = true;

      try
      {
        return meth.CallReflected(context, CallType.None, args);
      }
      catch (ArgumentTypeException ex)
      {
        return Closure.AssertionViolation(meth.ToString(), ex.Message, args);
      }
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

}
