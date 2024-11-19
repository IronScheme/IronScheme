#region License
/* Copyright (c) 2007-2016 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections.Generic;
using Microsoft.Scripting.Ast;

namespace IronScheme.Compiler
{
  abstract class TypedGenerator : SimpleGenerator
  {
    protected Type GetClosureType(CodeBlock cb)
    {
      Type[] types = GetTypeSpec(cb);

      var functype = GetGenericType("IronScheme.Runtime.Typed.TypedClosure", types);

      return functype as Type;
    }

    protected Type GetDelegateType(CodeBlock cb)
    {
      Type[] types = GetTypeSpec(cb);

      var functype = GetGenericType("IronScheme.Runtime.Typed.Func", types);

      return functype as Type;
    }

    protected static Type GetGenericType(string typename, Type[] types)
    {
      int l = types.Length;
      var functype = ClrGenerator.GetTypeFast(typename + "`" + l).MakeGenericType(types);
      return functype;
    }

    protected static Type[] GetTypeSpec(CodeBlock cb)
    {
      List<Type> types = new List<Type>();

      foreach (var v in cb.Parameters)
      {
        types.Add(v.Type);
      }

      types.Add(cb.ReturnType);
      return types.ToArray();
    }
  }
}
