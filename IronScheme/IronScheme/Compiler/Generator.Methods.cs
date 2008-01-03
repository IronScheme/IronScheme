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
using System.Reflection;
using IronScheme.Runtime;
using Microsoft.Scripting;

namespace IronScheme.Compiler
{
  partial class Generator
  {
    static readonly MethodInfo Builtins_Cons = typeof(Builtins).GetMethod("Cons", new Type[] { typeof(object) });
    protected static readonly MethodInfo Builtins_Cons2 = typeof(Builtins).GetMethod("Cons", new Type[] { typeof(object) , typeof(object)});
    static readonly MethodInfo Builtins_Append = typeof(Builtins).GetMethod("Append");
    static readonly MethodInfo Builtins_VectorAppend = typeof(Builtins).GetMethod("VectorAppend");
    static readonly MethodInfo Builtins_ToImproper = typeof(Builtins).GetMethod("ToImproper");
    protected static readonly MethodInfo Builtins_ListToVector = typeof(Builtins).GetMethod("ListToVector");

    protected readonly static MethodInfo Macro_MakeVarArgX = typeof(Runtime.Macro).GetMethod("MakeVarArgX");
    protected readonly static MethodInfo Macro_Make = typeof(Runtime.Macro).GetMethod("Make");

    protected static readonly MethodInfo Closure_Make = typeof(Closure).GetMethod("Make");
    static readonly MethodInfo Closure_MakeCase = typeof(Closure).GetMethod("MakeCase");
    static readonly MethodInfo Closure_MakeVarArgsX = typeof(Closure).GetMethod("MakeVarArgX");

    protected static readonly MethodInfo Builtins_IsTrue = typeof(Builtins).GetMethod("IsTrue");
    protected static readonly MethodInfo Builtins_Display = typeof(Builtins).GetMethod("Display", new Type[] { typeof(object) });

    protected static readonly MethodInfo ICallable_Call0 = typeof(ICallable).GetMethod("Call", new Type[] { });
    protected static readonly MethodInfo ICallable_Call1 = typeof(ICallable).GetMethod("Call", new Type[] { typeof(object), });
    protected static readonly MethodInfo ICallable_Call2 = typeof(ICallable).GetMethod("Call", new Type[] { typeof(object), typeof(object), });
    protected static readonly MethodInfo ICallable_Call3 = typeof(ICallable).GetMethod("Call", new Type[] { typeof(object), typeof(object), typeof(object), });
    protected static readonly MethodInfo ICallable_Call4 = typeof(ICallable).GetMethod("Call", new Type[] { typeof(object), typeof(object), typeof(object), typeof(object), });
    protected static readonly MethodInfo ICallable_Call5 = typeof(ICallable).GetMethod("Call", new Type[] { typeof(object), typeof(object), typeof(object), typeof(object), typeof(object), });
    protected static readonly MethodInfo ICallable_CallN = typeof(ICallable).GetMethod("Call", new Type[] { typeof(object[]), });

    protected static readonly MethodInfo CallTargetWithContext0_Invoke = typeof(CallTargetWithContext0).GetMethod("Invoke");
    protected static readonly MethodInfo CallTargetWithContext1_Invoke = typeof(CallTargetWithContext1).GetMethod("Invoke");
    protected static readonly MethodInfo CallTargetWithContext2_Invoke = typeof(CallTargetWithContext2).GetMethod("Invoke");
    protected static readonly MethodInfo CallTargetWithContext3_Invoke = typeof(CallTargetWithContext3).GetMethod("Invoke");
    protected static readonly MethodInfo CallTargetWithContext4_Invoke = typeof(CallTargetWithContext4).GetMethod("Invoke");
    protected static readonly MethodInfo CallTargetWithContext5_Invoke = typeof(CallTargetWithContext5).GetMethod("Invoke");
    protected static readonly MethodInfo CallTargetWithContextN_Invoke = typeof(CallTargetWithContextN).GetMethod("Invoke");

    protected static readonly MethodInfo CallTarget0_Invoke = typeof(CallTarget0).GetMethod("Invoke");
    protected static readonly MethodInfo CallTarget1_Invoke = typeof(CallTarget1).GetMethod("Invoke");
    protected static readonly MethodInfo CallTarget2_Invoke = typeof(CallTarget2).GetMethod("Invoke");
    protected static readonly MethodInfo CallTarget3_Invoke = typeof(CallTarget3).GetMethod("Invoke");
    protected static readonly MethodInfo CallTarget4_Invoke = typeof(CallTarget4).GetMethod("Invoke");
    protected static readonly MethodInfo CallTarget5_Invoke = typeof(CallTarget5).GetMethod("Invoke");
    protected static readonly MethodInfo CallTargetN_Invoke = typeof(CallTargetN).GetMethod("Invoke");

    protected static readonly MethodInfo Promise_Make = typeof(Promise).GetMethod("Make");

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
        default:
          return ICallable_CallN;
      }
    }

    protected static MethodInfo GetDirectCallable(bool needscontext, int arity)
    {
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
        default:
          return needscontext ? CallTargetWithContextN_Invoke : CallTargetN_Invoke;
      }
    }

  }
}
