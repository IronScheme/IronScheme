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
    [InlineEmitter("vector?")]
    public static Expression IsVector(Expression[] values)
    {
      return Ast.TypeIs(values[0], typeof(object[]));
    }

    [InlineEmitter("vector")]
    public static Expression Vector(Expression[] values)
    {
      return Ast.NewArray(typeof(object[]), values);
    }

    [InlineEmitter("vector-ref")]
    public static Expression VectorRef(Expression[] values)
    {
      return Ast.ArrayIndex(Ast.ConvertHelper(values[0], typeof(object[])), Ast.ConvertHelper(values[1], typeof(int)));
    }

    [InlineEmitter("vector-set!")]
    public static Expression VectorSet(Expression[] values)
    {
      return Ast.AssignArrayIndex(Ast.ConvertHelper(values[0], typeof(object[])), Ast.ConvertHelper(values[1], typeof(int)), values[2]);
    }

  }

  public partial class Builtins
  {
    /*
; 6.3.6
- vector?
- make-vector
- vector
- vector-length 
- vector-ref
- vector-set!

- vector->list 
- list->vector

- vector-fill! 
     */

    [Builtin("vector?")]
    public static object IsVector(object obj)
    {
      return GetBool(obj is object[]);
    }

    [Builtin("make-vector")]
    public static object[] MakeVector(object k)
    {
      return MakeVector(k, Unspecified);
    }

    [Builtin("make-vector")]
    public static object[] MakeVector(object K, object fill)
    {
      int k = RequiresNotNull<int>(K);
      object[] vector = new object[k];
      if (fill != null)
      {
        for (int i = 0; i < k; i++)
        {
          vector[i] = fill;
        }
      }
      return vector;
    }

    [Builtin("vector-fill!")]
    public static object VectorFill(object vec, object fill)
    {
      object[] vector = RequiresNotNull<object[]>(vec);
      for (int i = 0; i < vector.Length; i++)
      {
        vector[i] = fill;
      }
      return Unspecified;
    }

    [Builtin("vector")]
    public static object[] Vector(params object[] args)
    {
      return args;
    }

    [Builtin("vector-append")]
    public static object[] VectorAppend(params object[] args)
    {
      ArrayList all = new ArrayList();
      foreach (IEnumerable e in args)
      {
        foreach (object var in e)
        {
          all.Add(var);
        }
      }
      return all.ToArray();
    }

    [Builtin("vector-length")]
    public static object VectorLength(object vec)
    {
      object[] l = RequiresNotNull<object[]>(vec);
      return l.Length;
    }


    [Builtin("vector-ref")]
    public static object VectorRef(object vec, object K)
    {
      object[] l = RequiresNotNull<object[]>(vec);
      int k = RequiresNotNull<int>(K);
      return l[k];
    }

    [Builtin("vector-set!")]
    public static object VectorSet(object vec, object K, object value)
    {
      object[] l = RequiresNotNull<object[]>(vec);
      int k = RequiresNotNull<int>(K);
      l[k] = value;
      return Unspecified;
    }

    [Builtin("vector->list")]
    public static Cons VectorToList(object vec)
    {
      object[] l = Requires<object[]>(vec);
      return Runtime.Cons.FromArray(l);
    }

    [Builtin("list->vector")]
    public static object[] ListToVector(object list)
    {
      Cons e = Requires<Cons>(list);
      ArrayList v = new ArrayList();

      if (e != null)
      {
        RequiresCondition(e.IsProper, "must be a properlist");
        while (e != null)
        {
          v.Add(e.car);
          e = e.cdr as Cons;
        }
      }
      return v.ToArray();
    }

    [Builtin("vector-map")]
    public static object[] VectorMap(object proc, object vector)
    {
      ICallable p = RequiresNotNull<ICallable>(proc);
      object[] v = Requires<object[]>(vector);
      object[] r = new object[v.Length];
      for (int i = 0; i < v.Length; i++)
      {
        r[i] = p.Call(v[i]);
      }
      return r;
    }


    [Builtin("vector-map")]
    public static object[] VectorMap(object proc, params object[] lists)
    {
      int listcount = lists.Length;
      ICallable c = RequiresNotNull<ICallable>(proc);

      if (listcount > 0)
      {
        object[] output = new object[((object[])lists[0]).Length];
        for (int i = 0; i < output.Length; i++)
        {
          object[] args = new object[listcount];

          for (int j = 0; j < listcount; j++)
          {
            args[j] = ((object[])lists[j])[i];
          }

          output[i] = c.Call(args);
        }

        return output;
      }
      return lists;
    }

    [Builtin("vector-for-each")]
    public static object VectorForEach(object proc, object vector)
    {
      ICallable p = RequiresNotNull<ICallable>(proc);
      object[] v = Requires<object[]>(vector);
      
      for (int i = 0; i < v.Length; i++)
      {
        p.Call(v[i]);
      }
      return Unspecified;
    }

    [Builtin("vector-for-each")]
    public static object VectorForEach(object proc, params object[] lists)
    {
      int listcount = lists.Length;
      ICallable c = RequiresNotNull<ICallable>(proc);

      if (listcount > 0)
      {
        int ol = ((object[])lists[0]).Length;
        for (int i = 0; i < ol; i++)
        {
          object[] args = new object[listcount];

          for (int j = 0; j < listcount; j++)
          {
            args[j] = ((object[])lists[j])[i];
          }

          c.Call(args);
        }
      }
      return Unspecified;
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
    public static object[] VectorCopy(object vector)
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
    public static object[] VectorFilter(object proc, object vector)
    {
      ICallable p = RequiresNotNull<ICallable>(proc);
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
