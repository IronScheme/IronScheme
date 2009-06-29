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
using System.Collections;
using Microsoft.Scripting;
using System.IO;
using System.Diagnostics;

namespace IronScheme.Runtime
{
  [Serializable]
  public sealed class Cons : IEnumerable<object>
  {
    public object car;
    public object cdr;

    Cons()
      : this(null)
    {
    }

    public Cons(object car)
      : this(car, null)
    {
    }

    public Cons(object car, object cdr)
    {
      this.car = car;
      this.cdr = cdr;
    }

    public static Cons FromList(IEnumerable list)
    {
      if (list == null)
      {
        return null;
      }
      Cons first = null;
      Cons c = null;
      foreach (object var in list)
      {
        if (c == null)
        {
          first = c = new Cons(var);
        }
        else
        {
          Cons d = new Cons(var);
          c.cdr = d;
          c = d;
        }
      }
      return first;
    }

    [DebuggerStepThrough]
    public static Cons FromArray(params object[] args)
    {
      if (args == null)
      {
        return null;
      }
      if (args.Length == 0)
      {
        return null;
      }
      else if (args.Length == 1)
      {
        return new Cons(args[0]);
      }
      else
      {
        return Cons.FromList(args);
      }
    }

    internal int Length
    {
      get
      {
        int i = 0;
        foreach (var o in this)
        {
          i++;
        }
        return i;
      }
    }

    public string PrettyPrint
    {
      get { return ToPrettyString(); }
    }

    public string ToPrettyString()
    {
      StringWriter w = new StringWriter();
      ((Callable)Builtins.SymbolValue(SymbolTable.StringToObject("pretty-print"))).Call(this, w);
      return w.GetBuffer();
    }

    #region IEnumerable<object> Members

    // this only works with proper lists
    public IEnumerator<object> GetEnumerator()
    {
      Cons c = this;
      while (c != null)
      {
        yield return c.car;
        c = c.cdr as Cons;
      }
      yield break;
    }

    #endregion

    #region IEnumerable Members

    IEnumerator IEnumerable.GetEnumerator()
    {
      return GetEnumerator();
    }

    #endregion
  }
}
