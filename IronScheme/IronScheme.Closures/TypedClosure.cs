#region License
/* Copyright (c) 2007-2014 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System.Collections.Generic;
using System.Reflection;
using Microsoft.Scripting;
using System;

namespace IronScheme.Runtime.Typed
{
  public delegate R Func<R>();
  public delegate R Func<A1, R>(A1 a1);
  public delegate R Func<A1, A2, R>(A1 a1, A2 a2);
  public delegate R Func<A1, A2, A3, R>(A1 a1, A2 a2, A3 a3);
  public delegate R Func<A1, A2, A3, A4, R>(A1 a1, A2 a2, A3 a3, A4 a4);
  public delegate R Func<A1, A2, A3, A4, A5, R>(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5);
  public delegate R Func<A1, A2, A3, A4, A5, A6, R>(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6);
  public delegate R Func<A1, A2, A3, A4, A5, A6, A7, R>(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7);
  public delegate R Func<A1, A2, A3, A4, A5, A6, A7, A8, R>(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7, A8 a8);

  delegate void Action();
  delegate void Action<A1>(A1 a1);
  delegate void Action<A1, A2>(A1 a1, A2 a2);
  delegate void Action<A1, A2, A3>(A1 a1, A2 a2, A3 a3);
  delegate void Action<A1, A2, A3, A4>(A1 a1, A2 a2, A3 a3, A4 a4);
  delegate void Action<A1, A2, A3, A4, A5>(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5);
  delegate void Action<A1, A2, A3, A4, A5, A6>(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6);
  delegate void Action<A1, A2, A3, A4, A5, A6, A7>(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7);
  delegate void Action<A1, A2, A3, A4, A5, A6, A7, A8>(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7, A8 a8);

  internal static class Utils
  {
    static string GetTypeName(Type t)
    {
      // check if scheme record
      if (t.Namespace.StartsWith("record"))
      {
        return t.Name;
      }
      return t.ToString();
    }

    static T Unbox<T>(object o)
    {
      if (o is T)
      {
        return (T)o;
      }
      if (o == null && !typeof(T).IsValueType)
      {
        return (T)o;
      }
      return (T)Closure.AssertionViolation(false, "expected type: " + GetTypeName(typeof(T)), o);
    }

    static object Box<T>(T o)
    {
      if (typeof(T) == typeof(bool))
      {
        return RuntimeHelpers.BooleanToObject((bool)(object)o);
      }
      if (typeof(T) == typeof(SymbolId))
      {
        var s = (SymbolId)(object)o;
        return SymbolTable.Intern(s);
      }
      return o;
    }


    #region Typed Return Case - no tail calls

    public static Func<R> MakeTyped<R>(CallTarget0 f)
    {
      return () => Unbox<R>(f());
    }

    public static Func<A1, R> MakeTyped<A1, R>(CallTarget1 f)
    {
      return (a1) => Unbox<R>(f(a1));
    }

    public static Func<A1, A2, R> MakeTyped<A1, A2, R>(CallTarget2 f)
    {
      return (a1, a2) => Unbox<R>(f(a1,a2));
    }

    public static Func<A1, A2, A3, R> MakeTyped<A1, A2, A3, R>(CallTarget3 f)
    {
      return (a1, a2, a3) => Unbox<R>(f(a1, a2, a3));
    }

    public static Func<A1, A2, A3, A4, R> MakeTyped<A1, A2, A3, A4, R>(CallTarget4 f)
    {
      return (a1, a2, a3, a4) => Unbox<R>(f(a1, a2, a3, a4));
    }

    public static Func<A1, A2, A3, A4, A5, R> MakeTyped<A1, A2, A3, A4, A5, R>(CallTarget5 f)
    {
      return (a1, a2, a3, a4, a5) => Unbox<R>(f(a1, a2, a3, a4, a5));
    }

    public static Func<A1, A2, A3, A4, A5, A6, R> MakeTyped<A1, A2, A3, A4, A5, A6, R>(CallTarget6 f)
    {
      return (a1, a2, a3, a4, a5, a6) => Unbox<R>(f(a1, a2, a3, a4, a5, a6));
    }

    public static Func<A1, A2, A3, A4, A5, A6, A7, R> MakeTyped<A1, A2, A3, A4, A5, A6, A7, R>(CallTarget7 f)
    {
      return (a1, a2, a3, a4, a5, a6, a7) => Unbox<R>(f(a1, a2, a3, a4, a5, a6, a7));
    }

    public static Func<A1, A2, A3, A4, A5, A6, A7, A8, R> MakeTyped<A1, A2, A3, A4, A5, A6, A7, A8, R>(CallTarget8 f)
    {
      return (a1, a2, a3, a4, a5, a6, a7, a8) => Unbox<R>(f(a1, a2, a3, a4, a5, a6, a7, a8));
    }

    public static Action MakeVoidTyped(CallTarget0 f)
    {
      return () => f();
    }

    public static Action<A1> MakeVoidTyped<A1>(CallTarget1 f)
    {
      return (a1) => f(a1);
    }

    public static Action<A1, A2> MakeVoidTyped<A1, A2>(CallTarget2 f)
    {
      return (a1, a2) => f(a1, a2);
    }

    public static Action<A1, A2, A3> MakeVoidTyped<A1, A2, A3>(CallTarget3 f)
    {
      return (a1, a2, a3) => f(a1, a2, a3);
    }

    public static Action<A1, A2, A3, A4> MakeVoidTyped<A1, A2, A3, A4>(CallTarget4 f)
    {
      return (a1, a2, a3, a4) => f(a1, a2, a3, a4);
    }

    public static Action<A1, A2, A3, A4, A5> MakeVoidTyped<A1, A2, A3, A4, A5>(CallTarget5 f)
    {
      return (a1, a2, a3, a4, a5) => f(a1, a2, a3, a4, a5);
    }

    public static Action<A1, A2, A3, A4, A5, A6> MakeVoidTyped<A1, A2, A3, A4, A5, A6>(CallTarget6 f)
    {
      return (a1, a2, a3, a4, a5, a6) => f(a1, a2, a3, a4, a5, a6);
    }

    public static Action<A1, A2, A3, A4, A5, A6, A7> MakeVoidTyped<A1, A2, A3, A4, A5, A6, A7>(CallTarget7 f)
    {
      return (a1, a2, a3, a4, a5, a6, a7) => f(a1, a2, a3, a4, a5, a6, a7);
    }

    public static Action<A1, A2, A3, A4, A5, A6, A7, A8> MakeVoidTyped<A1, A2, A3, A4, A5, A6, A7, A8>(CallTarget8 f)
    {
      return (a1, a2, a3, a4, a5, a6, a7, a8) => f(a1, a2, a3, a4, a5, a6, a7, a8);
    }

    #endregion

    #region Typed Return Case - no tail calls

    public static CallTarget0 MakeUntyped<R>(Func<R> f)
    {
      return () => Box(f());
    }

    public static CallTarget1 MakeUntyped<A1, R>(Func<A1, R> f)
    {
      return (a1) =>
        Box(f(Unbox<A1>(a1)));
    }

    public static CallTarget2 MakeUntyped<A1, A2, R>(Func<A1, A2, R> f)
    {
      return (a1, a2) =>
        Box(f(Unbox<A1>(a1), Unbox<A2>(a2)));
    }

    public static CallTarget3 MakeUntyped<A1, A2, A3, R>(Func<A1, A2, A3, R> f)
    {
      return (a1, a2, a3) =>
        Box(f(Unbox<A1>(a1), Unbox<A2>(a2), Unbox<A3>(a3)));
    }

    public static CallTarget4 MakeUntyped<A1, A2, A3, A4, R>(Func<A1, A2, A3, A4, R> f)
    {
      return (a1, a2, a3, a4) =>
        Box(f(Unbox<A1>(a1), Unbox<A2>(a2), Unbox<A3>(a3), Unbox<A4>(a4)));
    }

    public static CallTarget5 MakeUntyped<A1, A2, A3, A4, A5, R>(Func<A1, A2, A3, A4, A5, R> f)
    {
      return (a1, a2, a3, a4, a5) =>
        Box(f(Unbox<A1>(a1), Unbox<A2>(a2), Unbox<A3>(a3), Unbox<A4>(a4), Unbox<A5>(a5)));
    }

    public static CallTarget6 MakeUntyped<A1, A2, A3, A4, A5, A6, R>(Func<A1, A2, A3, A4, A5, A6, R> f)
    {
      return (a1, a2, a3, a4, a5, a6) =>
        Box(f(Unbox<A1>(a1), Unbox<A2>(a2), Unbox<A3>(a3), Unbox<A4>(a4), Unbox<A5>(a5), Unbox<A6>(a6)));
    }

    public static CallTarget7 MakeUntyped<A1, A2, A3, A4, A5, A6, A7, R>(Func<A1, A2, A3, A4, A5, A6, A7, R> f)
    {
      return (a1, a2, a3, a4, a5, a6, a7) =>
        Box(f(Unbox<A1>(a1), Unbox<A2>(a2), Unbox<A3>(a3), Unbox<A4>(a4), Unbox<A5>(a5), Unbox<A6>(a6), Unbox<A7>(a7)));
    }

    public static CallTarget8 MakeUntyped<A1, A2, A3, A4, A5, A6, A7, A8, R>(Func<A1, A2, A3, A4, A5, A6, A7, A8, R> f)
    {
      return (a1, a2, a3, a4, a5, a6, a7, a8) =>
        Box(f(Unbox<A1>(a1), Unbox<A2>(a2), Unbox<A3>(a3), Unbox<A4>(a4), Unbox<A5>(a5), Unbox<A6>(a6), Unbox<A7>(a7), Unbox<A8>(a8)));
    }

    #endregion

    #region Untyped Return Case - tail calls

    public static CallTarget0 MakeUntypedTailCallSafe(Func<object> f)
    {
      return () => f();
    }

    public static CallTarget1 MakeUntypedTailCallSafe<A1>(Func<A1, object> f)
    {
      return (a1) =>
        f(Unbox<A1>(a1));
    }

    public static CallTarget2 MakeUntypedTailCallSafe<A1, A2>(Func<A1, A2, object> f)
    {
      return (a1, a2) =>
        f(Unbox<A1>(a1), Unbox<A2>(a2));
    }

    public static CallTarget3 MakeUntypedTailCallSafe<A1, A2, A3>(Func<A1, A2, A3, object> f)
    {
      return (a1, a2, a3) =>
        f(Unbox<A1>(a1), Unbox<A2>(a2), Unbox<A3>(a3));
    }

    public static CallTarget4 MakeUntypedTailCallSafe<A1, A2, A3, A4>(Func<A1, A2, A3, A4, object> f)
    {
      return (a1, a2, a3, a4) =>
        f(Unbox<A1>(a1), Unbox<A2>(a2), Unbox<A3>(a3), Unbox<A4>(a4));
    }

    public static CallTarget5 MakeUntypedTailCallSafe<A1, A2, A3, A4, A5>(Func<A1, A2, A3, A4, A5, object> f)
    {
      return (a1, a2, a3, a4, a5) =>
        f(Unbox<A1>(a1), Unbox<A2>(a2), Unbox<A3>(a3), Unbox<A4>(a4), Unbox<A5>(a5));
    }

    public static CallTarget6 MakeUntypedTailCallSafe<A1, A2, A3, A4, A5, A6>(Func<A1, A2, A3, A4, A5, A6, object> f)
    {
      return (a1, a2, a3, a4, a5, a6) =>
        f(Unbox<A1>(a1), Unbox<A2>(a2), Unbox<A3>(a3), Unbox<A4>(a4), Unbox<A5>(a5), Unbox<A6>(a6));
    }

    public static CallTarget7 MakeUntypedTailCallSafe<A1, A2, A3, A4, A5, A6, A7>(Func<A1, A2, A3, A4, A5, A6, A7, object> f)
    {
      return (a1, a2, a3, a4, a5, a6, a7) =>
        f(Unbox<A1>(a1), Unbox<A2>(a2), Unbox<A3>(a3), Unbox<A4>(a4), Unbox<A5>(a5), Unbox<A6>(a6), Unbox<A7>(a7));
    }

    public static CallTarget8 MakeUntypedTailCallSafe<A1, A2, A3, A4, A5, A6, A7, A8>(Func<A1, A2, A3, A4, A5, A6, A7, A8, object> f)
    {
      return (a1, a2, a3, a4, a5, a6, a7, a8) =>
        f(Unbox<A1>(a1), Unbox<A2>(a2), Unbox<A3>(a3), Unbox<A4>(a4), Unbox<A5>(a5), Unbox<A6>(a6), Unbox<A7>(a7), Unbox<A8>(a8));
    }

    #endregion

  }
  

  public class TypedClosure<R> : Closure, ITypedCallable<R>
  {
    readonly Func<R> typedtarget;
    readonly CallTarget0 untypedtarget;

    public TypedClosure(Func<R> target, CallTarget0 untypedtarget)
      : base(target, 0)
    {
      typedtarget = target;
      this.untypedtarget = untypedtarget;
    }

    public TypedClosure(Func<R> target)
      : base(target, 0)
    {
      typedtarget = target;
      untypedtarget = (typeof(R) == typeof(object)) ?
        Utils.MakeUntypedTailCallSafe((Func<object>)(object)target) :
        Utils.MakeUntyped((Func<R>)target);
    }

    public override MethodInfo[] Targets
    {
      get
      {
        List<MethodInfo> targets = new List<MethodInfo>();
        if (IsValid(typedtarget.Method) && typedtarget.Target == null)
        {
          targets.Add(typedtarget.Method);
        }
        if (IsValid(untypedtarget.Method))
        {
          targets.Add(untypedtarget.Method);
        }
        return targets.ToArray();
      }
    }

    public override object Call(object[] args)
    {
      if (args.Length != 0)
      {
        return AssertionViolation(GetWho(), string.Format("invalid argument count, expected {0} got {1}", paramcount, args.Length), args);
      }
      else
      {
        return untypedtarget();
      }
    }

    public override object Call()
    {
      return untypedtarget();
    }

    public R Invoke()
    {
      return typedtarget();
    }

    public override string ToString()
    {
      return typedtarget.Method.Name;
    }
  }

  public class TypedClosure<A1, R> : Closure, ITypedCallable<A1, R>
  {
    readonly Func<A1, R> typedtarget;
    readonly CallTarget1 untypedtarget;

    public TypedClosure(Func<A1, R> target, CallTarget1 untypedtarget)
      : base(target, 1)
    {
      typedtarget = target;
      this.untypedtarget = untypedtarget;
    }

    public TypedClosure(Func<A1, R> target)
      : base(target, 1)
    {
      typedtarget = target;
      untypedtarget = (typeof(R) == typeof(object)) ?
        Utils.MakeUntypedTailCallSafe((Func<A1,object>)(object)target) :
        Utils.MakeUntyped((Func<A1, R>)target);
    }

    public override MethodInfo[] Targets
    {
      get
      {
        List<MethodInfo> targets = new List<MethodInfo>();
        if (IsValid(typedtarget.Method) && typedtarget.Target == null)
        {
          targets.Add(typedtarget.Method);
        }
        if (IsValid(untypedtarget.Method))
        {
          targets.Add(untypedtarget.Method);
        }
        return targets.ToArray();
      }
    }

    public override object Call(object[] args)
    {
      if (args.Length != 1)
      {
        return AssertionViolation(GetWho(), string.Format("invalid argument count, expected {0} got {1}", 1, args.Length), args);
      }
      else
      {
        return untypedtarget(args[0]);
      }
    }

    public override object Call(object arg0)
    {
      return untypedtarget(arg0);
    }

    public R Invoke(A1 a1)
    {
      return typedtarget(a1);
    }

    public override string ToString()
    {
      return typedtarget.Method.Name;
    }

  }

  public class TypedClosure<A1, A2, R> : Closure, ITypedCallable<A1, A2, R>
  {
    readonly Func<A1, A2, R> typedtarget;
    readonly CallTarget2 untypedtarget;

    public TypedClosure(Func<A1, A2, R> target, CallTarget2 untypedtarget)
      : base(target, 2)
    {
      typedtarget = target;
      this.untypedtarget = untypedtarget;
    }

    public TypedClosure(Func<A1, A2, R> target)
      : base(target, 2)
    {
      typedtarget = target;
      untypedtarget = (typeof(R) == typeof(object)) ?
        Utils.MakeUntypedTailCallSafe((Func<A1, A2, object>)(object)target) :
        Utils.MakeUntyped((Func<A1, A2, R>)target);
    }

    public override MethodInfo[] Targets
    {
      get
      {
        List<MethodInfo> targets = new List<MethodInfo>();
        if (IsValid(typedtarget.Method) && typedtarget.Target == null)
        {
          targets.Add(typedtarget.Method);
        }
        if (IsValid(untypedtarget.Method))
        {
          targets.Add(untypedtarget.Method);
        }
        return targets.ToArray();
      }
    }

    public override object Call(object[] args)
    {
      if (args.Length != 2)
      {
        return AssertionViolation(GetWho(), string.Format("invalid argument count, expected {0} got {1}", 2, args.Length), args);
      }
      else
      {
        return untypedtarget(args[0], args[1]);
      }
    }

    public override object Call(object arg0, object arg1)
    {
      return untypedtarget(arg0, arg1);
    }

    public R Invoke(A1 a1, A2 a2)
    {
      return typedtarget(a1, a2);
    }

    public override string ToString()
    {
      return typedtarget.Method.Name;
    }

  }

  public class TypedClosure<A1, A2, A3, R> : Closure, ITypedCallable<A1, A2, A3, R>
  {
    readonly Func<A1, A2, A3, R> typedtarget;
    readonly CallTarget3 untypedtarget;

    public TypedClosure(Func<A1, A2, A3, R> target, CallTarget3 untypedtarget)
      : base(target, 3)
    {
      typedtarget = target;
      this.untypedtarget = untypedtarget;
    }

    public TypedClosure(Func<A1, A2, A3, R> target)
      : base(target, 3)
    {
      typedtarget = target;
      untypedtarget = (typeof(R) == typeof(object)) ?
        Utils.MakeUntypedTailCallSafe((Func<A1, A2, A3, object>)(object)target) :
        Utils.MakeUntyped((Func<A1, A2, A3, R>)target);
    }

    public override MethodInfo[] Targets
    {
      get
      {
        List<MethodInfo> targets = new List<MethodInfo>();
        if (IsValid(typedtarget.Method) && typedtarget.Target == null)
        {
          targets.Add(typedtarget.Method);
        }
        if (IsValid(untypedtarget.Method))
        {
          targets.Add(untypedtarget.Method);
        }
        return targets.ToArray();
      }
    }

    public override object Call(object[] args)
    {
      if (args.Length != 3)
      {
        return AssertionViolation(GetWho(), string.Format("invalid argument count, expected {0} got {1}", 3, args.Length), args);
      }
      else
      {
        return untypedtarget(args[0], args[1], args[2]);
      }
    }

    public override object Call(object arg0, object arg1, object arg2)
    {
      return untypedtarget(arg0, arg1, arg2);
    }

    public R Invoke(A1 a1, A2 a2, A3 a3)
    {
      return typedtarget(a1, a2, a3);
    }

    public override string ToString()
    {
      return typedtarget.Method.Name;
    }

  }

  public class TypedClosure<A1, A2, A3, A4, R> : Closure, ITypedCallable<A1, A2, A3, A4, R>
  {
    readonly Func<A1, A2, A3, A4, R> typedtarget;
    readonly CallTarget4 untypedtarget;

    public TypedClosure(Func<A1, A2, A3, A4, R> target, CallTarget4 untypedtarget)
      : base(target, 4)
    {
      typedtarget = target;
      this.untypedtarget = untypedtarget;
    }

    public TypedClosure(Func<A1, A2, A3, A4, R> target)
      : base(target, 4)
    {
      typedtarget = target;
      untypedtarget = (typeof(R) == typeof(object)) ?
        Utils.MakeUntypedTailCallSafe((Func<A1, A2, A3, A4, object>)(object)target) :
        Utils.MakeUntyped((Func<A1, A2, A3, A4, R>)target);
    }

    public override MethodInfo[] Targets
    {
      get
      {
        List<MethodInfo> targets = new List<MethodInfo>();
        if (IsValid(typedtarget.Method) && typedtarget.Target == null)
        {
          targets.Add(typedtarget.Method);
        }
        if (IsValid(untypedtarget.Method))
        {
          targets.Add(untypedtarget.Method);
        }
        return targets.ToArray();
      }
    }

    public override object Call(object[] args)
    {
      if (args.Length != 4)
      {
        return AssertionViolation(GetWho(), string.Format("invalid argument count, expected {0} got {1}", 4, args.Length), args);
      }
      else
      {
        return untypedtarget(args[0], args[1], args[2], args[3]);
      }
    }

    public override object Call(object arg0, object arg1, object arg2, object arg3)
    {
      return untypedtarget(arg0, arg1, arg2, arg3);
    }

    public R Invoke(A1 a1, A2 a2, A3 a3, A4 a4)
    {
      return typedtarget(a1, a2, a3, a4);
    }

    public override string ToString()
    {
      return typedtarget.Method.Name;
    }

  }

  public class TypedClosure<A1, A2, A3, A4, A5, R> : Closure, ITypedCallable<A1, A2, A3, A4, A5, R>
  {
    readonly Func<A1, A2, A3, A4, A5, R> typedtarget;
    readonly CallTarget5 untypedtarget;

    public TypedClosure(Func<A1, A2, A3, A4, A5, R> target, CallTarget5 untypedtarget)
      : base(target, 5)
    {
      typedtarget = target;
      this.untypedtarget = untypedtarget;
    }

    public TypedClosure(Func<A1, A2, A3, A4, A5, R> target)
      : base(target, 5)
    {
      typedtarget = target;
      untypedtarget = (typeof(R) == typeof(object)) ?
        Utils.MakeUntypedTailCallSafe((Func<A1, A2, A3, A4, A5, object>)(object)target) :
        Utils.MakeUntyped((Func<A1, A2, A3, A4, A5, R>)target); 
    }

    public override MethodInfo[] Targets
    {
      get
      {
        List<MethodInfo> targets = new List<MethodInfo>();
        if (IsValid(typedtarget.Method) && typedtarget.Target == null)
        {
          targets.Add(typedtarget.Method);
        }
        if (IsValid(untypedtarget.Method))
        {
          targets.Add(untypedtarget.Method);
        }
        return targets.ToArray();
      }
    }

    public override object Call(object[] args)
    {
      if (args.Length != 5)
      {
        return AssertionViolation(GetWho(), string.Format("invalid argument count, expected {0} got {1}", 5, args.Length), args);
      }
      else
      {
        return untypedtarget(args[0], args[1], args[2], args[3], args[4]);
      }
    }

    public override object Call(object arg0, object arg1, object arg2, object arg3, object arg4)
    {
      return untypedtarget(arg0, arg1, arg2, arg3, arg4);
    }

    public R Invoke(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5)
    {
      return typedtarget(a1, a2, a3, a4, a5);
    }

    public override string ToString()
    {
      return typedtarget.Method.Name;
    }

  }

  public class TypedClosure<A1, A2, A3, A4, A5, A6, R> : Closure, ITypedCallable<A1, A2, A3, A4, A5, A6, R>
  {
    readonly Func<A1, A2, A3, A4, A5, A6,  R> typedtarget;
    readonly CallTarget6 untypedtarget;

    public TypedClosure(Func<A1, A2, A3, A4, A5, A6, R> target, CallTarget6 untypedtarget)
      : base(target, 6)
    {
      typedtarget = target;
      this.untypedtarget = untypedtarget;
    }

    public TypedClosure(Func<A1, A2, A3, A4, A5, A6, R> target)
      : base(target, 6)
    {
      typedtarget = target;
      untypedtarget = (typeof(R) == typeof(object)) ?
        Utils.MakeUntypedTailCallSafe((Func<A1, A2, A3, A4, A5, A6, object>)(object)target) :
        Utils.MakeUntyped((Func<A1, A2, A3, A4, A5, A6, R>)target);
    }

    public override MethodInfo[] Targets
    {
      get
      {
        List<MethodInfo> targets = new List<MethodInfo>();
        if (IsValid(typedtarget.Method) && typedtarget.Target == null)
        {
          targets.Add(typedtarget.Method);
        }
        if (IsValid(untypedtarget.Method))
        {
          targets.Add(untypedtarget.Method);
        }
        return targets.ToArray();
      }
    }

    public override object Call(object[] args)
    {
      if (args.Length != 6)
      {
        return AssertionViolation(GetWho(), string.Format("invalid argument count, expected {0} got {1}", 6, args.Length), args);
      }
      else
      {
        return untypedtarget(args[0], args[1], args[2], args[3], args[4], args[5]);
      }
    }

    public override object Call(object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
    {
      return untypedtarget(arg0, arg1, arg2, arg3, arg4, arg5);
    }

    public R Invoke(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6)
    {
      return typedtarget(a1, a2, a3, a4, a5, a6);
    }

    public override string ToString()
    {
      return typedtarget.Method.Name;
    }

  }

  public class TypedClosure<A1, A2, A3, A4, A5, A6, A7, R> : Closure, ITypedCallable<A1, A2, A3, A4, A5, A6, A7, R>
  {
    readonly Func<A1, A2, A3, A4, A5, A6, A7, R> typedtarget;
    readonly CallTarget7 untypedtarget;

    public TypedClosure(Func<A1, A2, A3, A4, A5, A6, A7, R> target, CallTarget7 untypedtarget)
      : base(target, 7)
    {
      typedtarget = target;
      this.untypedtarget = untypedtarget;
    }

    public TypedClosure(Func<A1, A2, A3, A4, A5, A6, A7, R> target)
      : base(target, 7)
    {
      typedtarget = target;
      untypedtarget = (typeof(R) == typeof(object)) ?
        Utils.MakeUntypedTailCallSafe((Func<A1, A2, A3, A4, A5, A6, A7, object>)(object)target) :
        Utils.MakeUntyped((Func<A1, A2, A3, A4, A5, A6, A7, R>)target);
    }

    public override MethodInfo[] Targets
    {
      get
      {
        List<MethodInfo> targets = new List<MethodInfo>();
        if (IsValid(typedtarget.Method) && typedtarget.Target == null)
        {
          targets.Add(typedtarget.Method);
        }
        if (IsValid(untypedtarget.Method))
        {
          targets.Add(untypedtarget.Method);
        }
        return targets.ToArray();
      }
    }

    public override object Call(object[] args)
    {
      if (args.Length != 7)
      {
        return AssertionViolation(GetWho(), string.Format("invalid argument count, expected {0} got {1}", 7, args.Length), args);
      }
      else
      {
        return untypedtarget(args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
      }
    }

    public override object Call(object arg0, object arg1, object arg2, object arg3, object arg4, object arg5, object arg6)
    {
      return untypedtarget(arg0, arg1, arg2, arg3, arg4, arg5, arg6);
    }

    public R Invoke(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7)
    {
      return typedtarget(a1, a2, a3, a4, a5, a6, a7);
    }

    public override string ToString()
    {
      return typedtarget.Method.Name;
    }

  }

  public class TypedClosure<A1, A2, A3, A4, A5, A6, A7, A8, R> : Closure, ITypedCallable<A1, A2, A3, A4, A5, A6, A7, A8, R>
  {
    readonly Func<A1, A2, A3, A4, A5, A6, A7, A8, R> typedtarget;
    readonly CallTarget8 untypedtarget;

    public TypedClosure(Func<A1, A2, A3, A4, A5, A6, A7, A8, R> target, CallTarget8 untypedtarget)
      : base(target, 8)
    {
      typedtarget = target;
      this.untypedtarget = untypedtarget;
    }


    public TypedClosure(Func<A1, A2, A3, A4, A5, A6, A7, A8, R> target)
      : base(target, 8)
    {
      typedtarget = target;
      untypedtarget = (typeof(R) == typeof(object)) ?
        Utils.MakeUntypedTailCallSafe((Func<A1, A2, A3, A4, A5, A6, A7, A8, object>)(object)target) :
        Utils.MakeUntyped((Func<A1, A2, A3, A4, A5, A6, A7, A8, R>)target);
    }

    public override MethodInfo[] Targets
    {
      get
      {
        List<MethodInfo> targets = new List<MethodInfo>();
        if (IsValid(typedtarget.Method) && typedtarget.Target == null)
        {
          targets.Add(typedtarget.Method);
        }
        if (IsValid(untypedtarget.Method))
        {
          targets.Add(untypedtarget.Method);
        }
        return targets.ToArray();
      }
    }

    public override object Call(object[] args)
    {
      if (args.Length != 8)
      {
        return AssertionViolation(GetWho(), string.Format("invalid argument count, expected {0} got {1}", 8, args.Length), args);
      }
      else
      {
        return untypedtarget(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
      }
    }

    public override object Call(object arg0, object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7)
    {
      return untypedtarget(arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
    }

    public R Invoke(A1 a1, A2 a2, A3 a3, A4 a4, A5 a5, A6 a6, A7 a7, A8 a8)
    {
      return typedtarget(a1, a2, a3, a4, a5, a6, a7, a8);
    }

    public override string ToString()
    {
      return typedtarget.Method.Name;
    }

  }
}
