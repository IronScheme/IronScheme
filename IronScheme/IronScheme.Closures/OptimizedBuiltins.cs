#region License
/* Copyright (c) 2007-2015 Llewellyn Pritchard
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion


using System;
namespace IronScheme.Runtime
{
  public sealed class MultipleValues
  {
    readonly object[] values;

    public MultipleValues(params object[] values)
    {
      this.values = values;
    }

    internal int Length
    {
      get { return values.Length; }
    }

    internal object[] ToArray()
    {
      return values;
    }

    public object[] ToArray(int expects)
    {
      if (expects != values.Length)
      {
        Closure.AssertionViolation(false, string.Format("expected {0} arguments, got {1}", expects, values.Length), values);
      }

      return ToArray();
    }

    public object this[int index]
    {
      get { return values[index]; }
      set { values[index] = value; }
    }
  }

  public static class OptimizedBuiltins
  {
    [Procedure]
    public static object CallWithValues(object producer, object consumer)
    {
      Callable pro = (Callable)producer;
      Callable con = (Callable)consumer;

      object r = pro.Call();

      if (r is MultipleValues)
      {
        var mv = (MultipleValues)r;
        return con.Call(mv.ToArray());
      }

      return con.Call(r);
    }
  }
}
