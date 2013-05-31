#region License
/* Copyright (c) 2007-2013 Llewellyn Pritchard 
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
  static partial class BuiltinEmitters
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

    public static T[] ListToVector<T>(Cons list)
    {
      var l = new List<T>();
      while (list != null)
      {
        object o = list.car;
        if (o is T)
        {
          l.Add((T)o);
        }
        else
        {
          var t = typeof(T);
          if (o == null)
          {
            AssertionViolation("ListToVector", "type cannot be null", t, FALSE);
          }
          AssertionViolation("ListToVector", "not type of " + t.Namespace + "." + t.Name, o, o.GetType());
        }

        list = Requires<Cons>(list.cdr);

      }
      return l.ToArray();
    }

    public static double[] ListToFlonumVector(Cons list)
    {
      return ListToVector<double>(list);
    }

    public static int[] ListToFixnumVector(Cons list)
    {
      return ListToVector<int>(list);
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
