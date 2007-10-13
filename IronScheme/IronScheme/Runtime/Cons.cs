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
  public class Cons : IEnumerable
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

    //public Cons Reverse()
    //{
    //  object list = null;
    //  object o = this;
    //  while (o != null)
    //  {
    //    list = new Cons(((Cons)o).car, list);
    //    o = ((Cons)o).cdr;
    //  }
    //  return (Cons)list;
    //}


    //public static Cons FromList(IEnumerable list)
    //{
    //  if (list is Cons)
    //  {
    //    return list as Cons;
    //  }
    //  if (list == null)
    //  {
    //    return null;
    //  }
    //  object cdr = Builtins.Cdr(list);
    //  if (cdr is IEnumerable)
    //  {
    //    Cons c = new Cons(Builtins.Car(list), FromList(cdr as IEnumerable));
    //    return c;
    //  }
    //  else
    //  {
    //    Cons c = new Cons(Builtins.Car(list), cdr);
    //    return c;
    //  }
    //}

    //public static Cons FromArray(params object[] args)
    //{
    //  if (args == null)
    //  {
    //    return null;
    //  }
    //  if (args.Length == 0)
    //  {
    //    return null;
    //  }
    //  else if (args.Length == 1)
    //  {
    //    return new Cons(args[0]);
    //  }
    //  else
    //  {
    //    return Cons.FromList(args);
    //  }
    //}

    public bool IsPair
    {
      get { return !IsProper; }
    }

    public bool IsProper
    {
      get { return cdr is IEnumerable; }
    }


    IEnumerator IEnumerable.GetEnumerator()
    {
      yield return car;
      Cons c = cdr as Cons;
      if (c == null)
      {
        if (cdr == null)
        {
          yield break;
        }
        else
        {
          yield return cdr;
        }
      }
      else
      {
        foreach (object to in c)
        {
          yield return to;
        }
      }
    }

    public override string ToString()
    {
      List<string> v = new List<string>();
      Cons s = this as Cons;

      while (s != null)
      {
        v.Add(s.Car.ToString());
        if (s.Cdr != null && !(s.Cdr is Cons))
        {
          v.Add(".");
          v.Add(s.Cdr.ToString());
          break;
        }
        s = s.Cdr as Cons;
      }
      return string.Format("({0})", string.Join(" ", v.ToArray()));

      //return Builtins.WriteFormat(this);
    }
  }
}
