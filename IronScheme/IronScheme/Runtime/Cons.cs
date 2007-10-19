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

    public Cons SetCar(object car)
    {
      this.car = car;
      return this;
    }

    public Cons SetCdr(object cdr)
    {
      this.cdr = cdr;
      return this;
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

    public bool IsPair
    {
      get { return !IsProper; }
    }

    public bool IsProper
    {
      get { return cdr is Cons; }
    }

    public override string ToString()
    {
      return Builtins.DisplayFormat(this);
    }
  }
}
