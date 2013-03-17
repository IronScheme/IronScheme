#region License
/* Copyright (c) 2007-2013 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using Microsoft.Scripting;

namespace IronScheme.Runtime
{
  [Serializable]
  public class Cons : IEnumerable<object>
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

#if IDEA_EVER_GETS_A_SOLUTION
  public sealed class ImmutableCons : Cons
  {
    public ImmutableCons(object car)
      : base(car)
    {

    }

    public ImmutableCons(object car, object cdr) 
      : base(car, cdr)
    {

    }
  }
#endif
}
