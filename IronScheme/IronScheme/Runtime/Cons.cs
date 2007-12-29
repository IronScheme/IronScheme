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
  public sealed class Cons : IEnumerable, IEnumerable<object>
  {
    internal object car;
    internal object cdr;

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
      return Builtins.WriteFormat(this);
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

  class ConsCollectionWrapper : ICollection<object>
  {
    readonly Cons head;
    Cons last;

    public ConsCollectionWrapper(Cons head)
    {
      this.head = head;
      last = head;
    }

    #region ICollection<object> Members

    public void Add(object item)
    {
      Cons c = new Cons(item);
      last.cdr = c;
      last = c;
    }

    public void Clear()
    {
      head.car = head.cdr = null;
      last = head;
    }

    public bool Contains(object item)
    {
      foreach (object o in head)
      {
        if (Equals(o, item))
        {
          return true;
        }
      }
      return false;
    }

    public void CopyTo(object[] array, int arrayIndex)
    {
      List<object> l = new List<object>(head);
      l.CopyTo(array, arrayIndex);
    }

    public int Count
    {
      get 
      {
        int i = 0;
        Cons c = head;

        while (c != null)
        {
          i++;
          c = c.cdr as Cons;
        }
        return i;
      }
    }

    public bool IsReadOnly
    {
      get { return false; }
    }

    public bool Remove(object item)
    {
      throw new NotImplementedException();
    }

    #endregion

    #region IEnumerable<object> Members

    public IEnumerator<object> GetEnumerator()
    {
      return head.GetEnumerator();
    }

    #endregion

    #region IEnumerable Members

    IEnumerator IEnumerable.GetEnumerator()
    {
      return head.GetEnumerator();
    }

    #endregion
  }
}
