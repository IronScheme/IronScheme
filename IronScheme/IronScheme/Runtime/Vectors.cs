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

namespace IronScheme.Runtime
{
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
    public static bool IsVector(object obj)
    {
      return obj is object[];
    }

    [Builtin("make-vector")]
    public static object[] MakeVector(int k)
    {
      return MakeVector(k, null);
    }

    [Builtin("make-vector")]
    public static object[] MakeVector(int k, object fill)
    {
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
    public static int VectorLength(object vec)
    {
      object[] l = RequiresNotNull<object[]>(vec);
      return l.Length;
    }


    [Builtin("vector-ref")]
    public static object VectorRef(object vec, int k)
    {
      object[] l = RequiresNotNull<object[]>(vec);
      return l[k];
    }

    [Builtin("vector-set!")]
    public static object VectorSet(object vec, int k, object value)
    {
      object[] l = RequiresNotNull<object[]>(vec);
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
          v.Add(e.Car);
          e = e.Cdr as Cons;
        }
      }
      return v.ToArray();
    }
  }
}
