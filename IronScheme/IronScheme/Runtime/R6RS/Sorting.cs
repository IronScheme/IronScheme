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

namespace IronScheme.Runtime.R6RS
{
  public class Sorting : Builtins
  {
    [Builtin("vector-sort!")]
    public static object VectorSortD(object proc, object vec)
    {
      Callable c = RequiresNotNull<Callable>(proc);
      object[] v = RequiresNotNull<object[]>(vec);
      try
      {
        Array.Sort(v, delegate(object a, object b)
        {
          return ReferenceEquals(a, b) ? 0 : IsTrue(c.Call(a, b)) ? -1 : 1;
        });
      }
      catch (InvalidOperationException ex)
      {
        return AssertionViolation("vector-sort!", ex.Message, proc);
      }
      return Unspecified;
    }
  }
}

