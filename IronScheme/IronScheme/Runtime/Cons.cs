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

namespace IronScheme.Runtime
{
  public class Cons
  {
    object car;
    object cdr;

    internal object Car
    {
      get { return car; }
      set { car = value; }
    }

    internal object Cdr
    {
      get {return cdr;}
      set { cdr = value; }
    }

    Cons() : this(null)
    {
    }

    public Cons(object car) : this(car, null)
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

    internal bool IsProper
    {
      get { return IsProperList(null); }
    }

    bool IsProperList(Cons root)
    {
      if (this == root)
      {
        return false;
      }
      return cdr == null || (cdr is Cons && ((Cons)cdr).IsProperList(root ?? this));
    }

    public override string ToString()
    {
      return Builtins.DisplayFormat(this);
    }
  }
}
