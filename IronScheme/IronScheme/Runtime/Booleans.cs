#region License
/* Copyright (c) 2007-2014 Llewellyn Pritchard 
 * All rights reserved.
 * This source code is subject to terms and conditions of the BSD License.
 * See docs/license.txt. */
#endregion

using System.Collections.Generic;
namespace IronScheme.Runtime
{
  public partial class Builtins
  {
    [Builtin("not", AllowConstantFold=true)]
    public static object Not(object obj)
    {
      return GetBool(!IsTrue(obj));
    }

    protected internal static object GetBool(bool v)
    {
      return v ? TRUE : FALSE;
    }
  }
}
