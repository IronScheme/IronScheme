#region License
/* Copyright (c) 2007,2008,2009,2010 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using System.Reflection;
using Microsoft.Scripting;
using Microsoft.Scripting.Actions;

namespace IronScheme.Runtime
{
  public sealed class BuiltinMethod : Callable
  {
    readonly MethodBinder meth;
    readonly MethodBase[] methods;
    readonly Dictionary<int, Callable> cache = new Dictionary<int, Callable>();

    public MethodBinder Binder
    {
      get { return meth; }
    }

    readonly string name;

    public string Name
    {
      get { return name; }
    }

    public override object Arity
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
        if (arities.Count == 0)
        {
          return Closure.Unspecified;
        }
        else if (arities.Count == 1)
        {
          return arities[0];
        }
        else
        {
          return new MultipleValues(arities.ToArray());
        }
      }
    }

    public override object Form
    {
      get 
      {
        List<object> forms = new List<object>();
        bool improper = false;
        foreach (MethodBase m in methods)
        {
          List<object> form = new List<object>();
          form.Add(SymbolTable.StringToObject(ToString()));

          ParameterInfo[] pis = m.GetParameters();

          foreach (ParameterInfo pi in pis)
          {
            if (pi.ParameterType != typeof(CodeContext))
            {
              form.Add(SymbolTable.StringToObject(pi.Name));
            }
          }

          if (pis.Length > 0 && pis[pis.Length - 1].IsDefined(typeof(ParamArrayAttribute), false))
          {
            improper = true;
          }
          if (improper)
          {
            forms.Add(Closure.ConsStarFromArray(form.ToArray()));
          }
          else
          {
            forms.Add(Closure.ConsFromArray(form.ToArray()));
          }
        }
        switch (forms.Count)
        {
          case 0:
            return Closure.Unspecified;
          case 1:
            return forms[0];
          default:
            return new MultipleValues(forms.ToArray());
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

    bool foldable;

    public BuiltinMethod(string name, MethodBase[] mg)
      : this(name, mg, false)
    {
    }

    public BuiltinMethod(string name, MethodBase[] mg, bool foldable)
    {
      this.name = name;
      this.methods = mg;
      meth = MethodBinder.MakeBinder(binder, name, mg, BinderType.Normal);
      this.foldable = foldable;

    }

    bool baked = false;

    public override object Call(object[] args)
    {
      if (args == null)
      {
        throw new ArgumentNullException("args cannot be null");
      }

      int nargs = args.Length;

      Callable c;

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

            Type dt = (nargs > 8 || IsParams(mb)) ? // for mono
              typeof(CallTargetN) : CallTargets.GetTargetType(needContext, nargs, false);
            Delegate d = Delegate.CreateDelegate(dt, mb as MethodInfo, false);
            if (d == null)
            {
              d = Delegate.CreateDelegate(needContext ? typeof(CallTargetWithContextN) : typeof(CallTargetN), mb as MethodInfo, false);
            }

            if (d != null)
            {
              cache[nargs] = c = Closure.Create(context, d);
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
        //if (this == Closure.CPSPrim)
        //{
        //  List<object> aargs = new List<object>(args);
        //  aargs.Insert(0, Closure.IdentityForCPS);
        //  return meth.CallReflected(context, CallType.None, aargs.ToArray());
        //}
        // DO NOT WANT!!!!
        //Debugger.Break();
        return meth.CallReflected(context, CallType.None, args);
      }
      catch (ArgumentTypeException ex)
      {
        return Closure.AssertionViolation(meth.ToString(), ex.Message, args);
      }
    }

    bool IsParams(MethodBase mb)
    {
      foreach (var pi in mb.GetParameters())
      {
        if (pi.IsDefined(typeof(ParamArrayAttribute), false))
        {
          return true;
        }
      }

      return false;
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

    public override object Call()
    {
      Callable c;
      if (cache.TryGetValue(0, out c))
      {
        return c.Call();
      }
      return Call(new object[0]);
    }

    public override object Call(object arg1)
    {
      Callable c;
      if (cache.TryGetValue(1, out c))
      {
        return c.Call(arg1);
      }
      return Call(new object[] { arg1 });
    }

    public override object Call(object arg1, object arg2)
    {
      Callable c;
      if (cache.TryGetValue(2, out c))
      {
        return c.Call(arg1, arg2);
      }
      return Call(new object[] { arg1, arg2 });
    }

    public override object Call(object arg1, object arg2, object arg3)
    {
      Callable c;
      if (cache.TryGetValue(3, out c))
      {
        return c.Call(arg1, arg2, arg3);
      }
      return Call(new object[] { arg1, arg2, arg3 });
    }

    public override object Call(object arg1, object arg2, object arg3, object arg4)
    {
      Callable c;
      if (cache.TryGetValue(4, out c))
      {
        return c.Call(arg1, arg2, arg3, arg4);
      }
      return Call(new object[] { arg1, arg2, arg3, arg4 });
    }

    public override object Call(object arg1, object arg2, object arg3, object arg4, object arg5)
    {
      Callable c;
      if (cache.TryGetValue(5, out c))
      {
        return c.Call(arg1, arg2, arg3, arg4, arg5);
      }
      return Call(new object[] { arg1, arg2, arg3, arg4, arg5 });
    }

    public override object Call(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6)
    {
      Callable c;
      if (cache.TryGetValue(6, out c))
      {
        return c.Call(arg1, arg2, arg3, arg4, arg5, arg6);
      }
      return Call(new object[] { arg1, arg2, arg3, arg4, arg5, arg6 });
    }

    public override object Call(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7)
    {
      Callable c;
      if (cache.TryGetValue(7, out c))
      {
        return c.Call(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
      }
      return Call(new object[] { arg1, arg2, arg3, arg4, arg5, arg6, arg7 });
    }

    public override object Call(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7, object arg8)
    {
      Callable c;
      if (cache.TryGetValue(8, out c))
      {
        return c.Call(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
      }
      return Call(new object[] { arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8 });
    }


    #endregion



  }

}
