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
    static readonly MethodInfo Builtins_Cons2 = typeof(Builtins).GetMethod("Cons", new Type[] { typeof(object) , typeof(object)});
    static readonly MethodInfo Builtins_Append = typeof(Builtins).GetMethod("Append");
    static readonly MethodInfo Builtins_VectorAppend = typeof(Builtins).GetMethod("VectorAppend");
    static readonly MethodInfo Builtins_ToImproper = typeof(Builtins).GetMethod("ToImproper");
    static readonly MethodInfo Builtins_ListToVector = typeof(Builtins).GetMethod("ListToVector");

    readonly static MethodInfo Macro_MakeVarArgX = typeof(Runtime.Macro).GetMethod("MakeVarArgX");
    readonly static MethodInfo Macro_Make = typeof(Runtime.Macro).GetMethod("Make");

    static readonly MethodInfo Closure_Make = typeof(Closure).GetMethod("Make");
    static readonly MethodInfo Closure_MakeVarArgsX = typeof(Closure).GetMethod("MakeVarArgX");

    protected static readonly MethodInfo Builtins_IsTrue = typeof(Builtins).GetMethod("IsTrue");
    protected static readonly MethodInfo Builtins_Display = typeof(Builtins).GetMethod("Display", new Type[] { typeof(object) });

    static readonly MethodInfo ICallableWithCodeContext_Call = typeof(ICallableWithCodeContext).GetMethod("Call");

  }
}
