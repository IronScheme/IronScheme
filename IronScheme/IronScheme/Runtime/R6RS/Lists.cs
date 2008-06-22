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
using Microsoft.Scripting.Generation;
using Microsoft.Scripting;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Reflection.Emit;
using System.Collections;

namespace IronScheme.Runtime.R6RS
{
  public class Lists : Builtins
  {
    [Builtin("find")]
    public static object Find(object proc, object list)
    {
      ICallable p = RequiresNotNull<ICallable>(proc);
      Cons c = Requires<Runtime.Cons>(list);

      while (c != null)
      {
        if (IsTrue(p.Call(c.car)))
        {
          return c.car;
        }
        c = c.cdr as Cons;
      }

      return FALSE;
    }


    [Builtin("remp")]
    public static object Remp(object proc, object list)
    {
      ICallable p = RequiresNotNull<ICallable>(proc);
      Cons e = Requires<Runtime.Cons>(list);
      Cons h = null, head = null;

      while (e != null)
      {
        if (!IsTrue(p.Call(e.car)))
        {
          if (h == null)
          {
            h = head = new Cons(e.car);
          }
          else
          {
            h = (Cons)(h.cdr = new Cons(e.car));
          }
        }
        e = e.cdr as Cons;
      }
      return head;
    }

    [Builtin("filter")]
    public static object Filter(object proc, object list)
    {
      ICallable p = RequiresNotNull<ICallable>(proc);
      Cons e = Requires<Runtime.Cons>(list);
      Cons h = null, head = null;

      while (e != null)
      {
        if (IsTrue(p.Call(e.car)))
        {
          if (h == null)
          {
            h = head = new Cons(e.car);
          }
          else
          {
            h = (Cons)(h.cdr = new Cons(e.car));
          }
        }
        e = e.cdr as Cons;
      }
      return head;
    }

    [Builtin("partition")]
    public static object Partition(object proc, object list)
    {
      ICallable p = RequiresNotNull<ICallable>(proc);
      Cons e = Requires<Runtime.Cons>(list);
      Cons h = null, head = null;
      Cons h2 = null, head2 = null;

      while (e != null)
      {
        if (IsTrue(p.Call(e.car)))
        {
          if (h == null)
          {
            h = head = new Cons(e.car);
          }
          else
          {
            h = (Cons)(h.cdr = new Cons(e.car));
          }
        }
        else
        {
          if (h2 == null)
          {
            h2 = head2 = new Cons(e.car);
          }
          else
          {
            h2 = (Cons)(h2.cdr = new Cons(e.car));
          }
        }
        e = e.cdr as Cons;
      }
      return Values(head, head2);
    }

    //extra, not R6RS
    [Builtin("group-by")]
    public static object GroupBy(object proc, object list)
    {
      ICallable p = RequiresNotNull<ICallable>(proc);
      Cons e = Requires<Runtime.Cons>(list);
      Hashtable result = new Hashtable();

      while (e != null)
      {
        object key = p.Call(e.car);

        object c = result[key];
        result[key] = new Cons(e.car, c);

        e = e.cdr as Cons;
      }

      Hashtable final = new Hashtable(result.Count);

      //now reverse the values
      foreach (DictionaryEntry de in result)
      {
        final[de.Key] = Reverse(de.Value);
      }

      return final;
    }


  }
}

