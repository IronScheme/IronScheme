#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Permissive License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Permissive License.
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
  public static partial class Builtins
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
      return obj is IList;
    }

    [Builtin("make-vector")]
    public static IList MakeVector(int k)
    {
      return MakeVector(k, null);
    }

    [Builtin("make-vector")]
    public static IList MakeVector(int k, object fill)
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
      IList vector = RequiresNotNull<IList>(vec);
      for (int i = 0; i < vector.Count; i++)
      {
        vector[i] = fill;
      }
      return Unspecified;
    }

    [Builtin("vector")]
    public static IList Vector(params object[] args)
    {
      return args;
    }

    [Builtin("vector-append")]
    public static IList VectorAppend(params object[] args)
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
      IList l = RequiresNotNull<IList>(vec);
      return l.Count;
    }


    [Builtin("vector-ref")]
    public static object VectorRef(object vec, int k)
    {
      IList l = RequiresNotNull<IList>(vec);
      return l[k];
    }

    [Builtin("vector-set!")]
    public static object VectorSet(object vec, int k, object value)
    {
      IList l = RequiresNotNull<IList>(vec);
      l[k] = value;
      return Unspecified;
    }

    [Builtin("vector->list")]
    public static IEnumerable VectorToList(object vec)
    {
      return Runtime.Cons.FromList(vec as IList);
    }

    [Builtin("list->vector")]
    public static IList ListToVector(object list)
    {
      IEnumerable e = Requires<IEnumerable>(list);
      ArrayList v = new ArrayList();

      if (e != null)
      {
        foreach (object var in e)
        {
          v.Add(var);
        }
      }
      return v.ToArray();
    }
  }
}
