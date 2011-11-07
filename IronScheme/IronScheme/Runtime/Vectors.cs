#region License
/* Copyright (c) 2007,2008,2009,2010,2011 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections;
using System.Collections.Generic;
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
      foreach (var e in args)
      {
        all.AddRange(RequiresNotNull<object[]>(e));
      }
      return all.ToArray();
    }

    internal static Cons VectorToList(object[] vec)
    {
      return Runtime.Cons.FromArray(vec);
    }

    internal static object[] ListToVector(Cons e)
    {
      ArrayList v = new ArrayList();

      while (e != null)
      {
        v.Add(e.car);
        e = Requires<Cons>(e.cdr);
      }

      return v.ToArray();
    }

    public static object[] ListToVector(object args)
    {
      Cons e = Requires<Cons>(args);
      return ListToVector(e);
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
    [UnspecifiedReturn]
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
      object[] v = RequiresNotNull<object[]>(vector);

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
