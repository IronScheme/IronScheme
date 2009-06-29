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
using Microsoft.Scripting.Math;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Collections;
using IronScheme.Compiler;
using Microsoft.Scripting.Ast;

namespace IronScheme.Runtime
{
  public static partial class BuiltinEmitters
  {
    [InlineEmitter("vector")]
    public static Expression Vector(Expression[] values)
    {
      return Ast.NewArray(typeof(object[]), values);
    }

  }

  public partial class Builtins
  {
    [Builtin("vector")]
    public static object Vector(params object[] args)
    {
      return args;
    }

    [Builtin("vector-append")]
    public static object VectorAppend(params object[] args)
    {
      ArrayList all = new ArrayList();
      foreach (object[] e in args)
      {
        all.AddRange(e);
      }
      return all.ToArray();
    }

    internal static Cons VectorToList(object[] vec)
    {
      object[] l = RequiresNotNull<object[]>(vec);
      return Runtime.Cons.FromArray(l);
    }

    public static object[] ListToVector(Cons e)
    {
      ArrayList v = new ArrayList();

      while (e != null)
      {
        v.Add(e.car);
        e = e.cdr as Cons;
      }

      return v.ToArray();
    }

    [Builtin("vector-binary-search")]
    public static object VectorBinarySearch(object vector, object obj)
    {
      Array v = RequiresNotNull<Array>(vector);
      return Array.BinarySearch(v, obj);
    }

    [Builtin("vector-index-of")]
    public static object VectorIndexOf(object vector, object obj)
    {
      Array v = RequiresNotNull<Array>(vector);
      return Array.IndexOf(v, obj);
    }

    [Builtin("vector-contains?")]
    public static object VectorContains(object vector, object obj)
    {
      Array v = RequiresNotNull<Array>(vector);
      return Array.IndexOf(v, obj) >= 0;
    }

    [Builtin("vector-copy")]
    public static object VectorCopy(object vector)
    {
      Array v = RequiresNotNull<Array>(vector);
      return v.Clone() as object[];
    }

    [Builtin("vector-reverse!")]
    public static object VectorReverse(object vector)
    {
      Array v = RequiresNotNull<Array>(vector);
      Array.Reverse(v);
      return Unspecified;
    }

    [Builtin("vector-filter")]
    public static object VectorFilter(object proc, object vector)
    {
      Callable p = RequiresNotNull<Callable>(proc);
      object[] v = Requires<object[]>(vector);

      List<object> output = new List<object>();
      
      for (int i = 0; i < v.Length; i++)
      {
        if (IsTrue(p.Call(v[i])))
        {
          output.Add(v[i]);
        }
      }
      return output.ToArray();
    }

  }
}
