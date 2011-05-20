#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Reflection;
using IronScheme.Runtime;
using Microsoft.Scripting;
using BigInteger = Oyster.Math.IntX;

namespace IronScheme.Compiler
{
  partial class Generator
  {
    //protected static MethodInfo Builtins_List = typeof(Builtins).GetMethod("List", new Type[] { typeof(object[]) });
    static readonly MethodInfo Builtins_Cons = typeof(Builtins).GetMethod("Cons", new Type[] { typeof(object) });
    protected static readonly MethodInfo Builtins_Cons2 = typeof(Builtins).GetMethod("Cons", new Type[] { typeof(object) , typeof(object)});
    static readonly MethodInfo Builtins_Append0 = typeof(Builtins).GetMethod("Append", new Type[] { });
    static readonly MethodInfo Builtins_Append1 = typeof(Builtins).GetMethod("Append", new Type[] { typeof(object) });
    static readonly MethodInfo Builtins_Append2 = typeof(Builtins).GetMethod("Append", new Type[] { typeof(object), typeof(object) });
    static readonly MethodInfo Builtins_AppendX = typeof(Builtins).GetMethod("Append", new Type[] { typeof(object[]) });
    static readonly MethodInfo Builtins_VectorAppend = typeof(Builtins).GetMethod("VectorAppend");
    static readonly MethodInfo Builtins_ToImproper = typeof(Builtins).GetMethod("ToImproper");

    protected static readonly MethodInfo Closure_Make = typeof(Closure).GetMethod("Create", new Type[] { typeof(object), typeof(Delegate) });
    protected static readonly MethodInfo Closure_MakeCase = typeof(Closure).GetMethod("CreateCase");
    protected static readonly MethodInfo Closure_MakeVarArgsX = typeof(Closure).GetMethod("CreateVarArgX");
    protected static readonly MethodInfo Closure_MakeTypedCase = typeof(Closure).GetMethod("CreateTypedCase");

    protected internal static readonly MethodInfo Builtins_IsTrue = typeof(Builtins).GetMethod("IsTrue");
    protected internal static readonly MethodInfo BooleanToObject = typeof(RuntimeHelpers).GetMethod("BooleanToObject");
    protected static readonly MethodInfo Builtins_Display = typeof(Builtins).GetMethod("Display", new Type[] { typeof(object) });

    protected static readonly MethodInfo ICallable_Call0 = typeof(Callable).GetMethod("Call", new Type[] { });
    protected static readonly MethodInfo ICallable_Call1 = typeof(Callable).GetMethod("Call", new Type[] { typeof(object), });
    protected static readonly MethodInfo ICallable_Call2 = typeof(Callable).GetMethod("Call", new Type[] { typeof(object), typeof(object), });
    protected static readonly MethodInfo ICallable_Call3 = typeof(Callable).GetMethod("Call", new Type[] { typeof(object), typeof(object), typeof(object), });
    protected static readonly MethodInfo ICallable_Call4 = typeof(Callable).GetMethod("Call", new Type[] { typeof(object), typeof(object), typeof(object), typeof(object), });
    protected static readonly MethodInfo ICallable_Call5 = typeof(Callable).GetMethod("Call", new Type[] { typeof(object), typeof(object), typeof(object), typeof(object), typeof(object), });
    protected static readonly MethodInfo ICallable_Call6 = typeof(Callable).GetMethod("Call", new Type[] { typeof(object), typeof(object), typeof(object), typeof(object), typeof(object), typeof(object), });
    protected static readonly MethodInfo ICallable_Call7 = typeof(Callable).GetMethod("Call", new Type[] { typeof(object), typeof(object), typeof(object), typeof(object), typeof(object), typeof(object), typeof(object), });
    protected static readonly MethodInfo ICallable_Call8 = typeof(Callable).GetMethod("Call", new Type[] { typeof(object), typeof(object), typeof(object), typeof(object), typeof(object), typeof(object), typeof(object), typeof(object), });
    protected internal static readonly MethodInfo ICallable_CallN = typeof(Callable).GetMethod("Call", new Type[] { typeof(object[]), });

    protected static readonly MethodInfo CallTargetWithContext0_Invoke = typeof(CallTargetWithContext0).GetMethod("Invoke");
    protected static readonly MethodInfo CallTargetWithContext1_Invoke = typeof(CallTargetWithContext1).GetMethod("Invoke");
    protected static readonly MethodInfo CallTargetWithContext2_Invoke = typeof(CallTargetWithContext2).GetMethod("Invoke");
    protected static readonly MethodInfo CallTargetWithContext3_Invoke = typeof(CallTargetWithContext3).GetMethod("Invoke");
    protected static readonly MethodInfo CallTargetWithContext4_Invoke = typeof(CallTargetWithContext4).GetMethod("Invoke");
    protected static readonly MethodInfo CallTargetWithContext5_Invoke = typeof(CallTargetWithContext5).GetMethod("Invoke");
    protected static readonly MethodInfo CallTargetWithContext6_Invoke = typeof(CallTargetWithContext6).GetMethod("Invoke");
    protected static readonly MethodInfo CallTargetWithContext7_Invoke = typeof(CallTargetWithContext7).GetMethod("Invoke");
    protected static readonly MethodInfo CallTargetWithContext8_Invoke = typeof(CallTargetWithContext8).GetMethod("Invoke");
    protected static readonly MethodInfo CallTargetWithContextN_Invoke = typeof(CallTargetWithContextN).GetMethod("Invoke");

    protected static readonly MethodInfo CallTarget0_Invoke = typeof(CallTarget0).GetMethod("Invoke");
    protected static readonly MethodInfo CallTarget1_Invoke = typeof(CallTarget1).GetMethod("Invoke");
    protected static readonly MethodInfo CallTarget2_Invoke = typeof(CallTarget2).GetMethod("Invoke");
    protected static readonly MethodInfo CallTarget3_Invoke = typeof(CallTarget3).GetMethod("Invoke");
    protected static readonly MethodInfo CallTarget4_Invoke = typeof(CallTarget4).GetMethod("Invoke");
    protected static readonly MethodInfo CallTarget5_Invoke = typeof(CallTarget5).GetMethod("Invoke");
    protected static readonly MethodInfo CallTarget6_Invoke = typeof(CallTarget6).GetMethod("Invoke");
    protected static readonly MethodInfo CallTarget7_Invoke = typeof(CallTarget7).GetMethod("Invoke");
    protected static readonly MethodInfo CallTarget8_Invoke = typeof(CallTarget8).GetMethod("Invoke");
    protected static readonly MethodInfo CallTargetN_Invoke = typeof(CallTargetN).GetMethod("Invoke");


    protected static readonly ConstructorInfo Fraction_New = typeof(Fraction).GetConstructor(new Type[] { 
#if BIGFRACTION
      typeof(BigInteger), typeof(BigInteger) 
#else
      typeof(long), typeof(long) 
#endif
    });

    protected internal static MethodInfo GetCallable(int arity)
    {
      switch (arity)
      {
        case 0:
          return ICallable_Call0;
        case 1:
          return ICallable_Call1;
        case 2:
          return ICallable_Call2;
        case 3:
          return ICallable_Call3;
        case 4:
          return ICallable_Call4;
        case 5:
          return ICallable_Call5;
        case 6:
          return ICallable_Call6;
        case 7:
          return ICallable_Call7;
        case 8:
          return ICallable_Call8;
        default:
          return ICallable_CallN;
      }
    }

    protected static MethodInfo GetDirectCallable(bool needscontext, int arity, Type dt)
    {
      if (dt.IsGenericType)
      {
        return dt.GetMethod("Invoke");
      }
      switch (arity)
      {
        case 0:
          return needscontext ? CallTargetWithContext0_Invoke : CallTarget0_Invoke;
        case 1:
          return needscontext ? CallTargetWithContext1_Invoke : CallTarget1_Invoke;
        case 2:
          return needscontext ? CallTargetWithContext2_Invoke : CallTarget2_Invoke;
        case 3:
          return needscontext ? CallTargetWithContext3_Invoke : CallTarget3_Invoke;
        case 4:
          return needscontext ? CallTargetWithContext4_Invoke : CallTarget4_Invoke;
        case 5:
          return needscontext ? CallTargetWithContext5_Invoke : CallTarget5_Invoke;
        case 6:
          return needscontext ? CallTargetWithContext6_Invoke : CallTarget6_Invoke;
        case 7:
          return needscontext ? CallTargetWithContext7_Invoke : CallTarget7_Invoke;
        case 8:
          return needscontext ? CallTargetWithContext8_Invoke : CallTarget8_Invoke;
        default:
          return needscontext ? CallTargetWithContextN_Invoke : CallTargetN_Invoke;
      }
    }

  }
}
