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
using Microsoft.Scripting.Math;
using System.Reflection;
using Microsoft.Scripting.Utils;
using System.Collections;
using Microsoft.Scripting;
using System.IO;

namespace IronScheme.Runtime
{
  public partial class Builtins
  {
    /*

; 6.3.1
- not
- boolean?

     */

    /// <summary>
    /// Determines whether the specified obj is boolean.
    /// </summary>
    /// <param name="obj">The obj.</param>
    /// <returns>
    /// 	<c>true</c> if the specified obj is boolean; otherwise, <c>false</c>.
    /// </returns>
    [Builtin("boolean?")]
    public static object IsBoolean(object obj)
    {
      return obj is bool; 
    }


    /// <summary>
    /// Nots the specified obj.
    /// </summary>
    /// <param name="obj">The obj.</param>
    /// <returns></returns>
    [Builtin("not")]
    public static object Not(object obj)
    {
      if (obj is bool)
      {
        return !(bool)obj;
      }
      return !IsTrue(obj);
    }


#if R6RS
    [Builtin("boolean=?")]
    public static object IsAllSameBoolean(object obj1, object obj2)
    {
      return Equals(obj1, obj2);
    }

    [Builtin("boolean=?")]
    public static object IsAllSameBoolean(object obj1, object obj2, object obj3, params object[] rest)
    {
      bool h = Equals(obj1, obj2) && Equals(obj2, obj3);
      if (h)
      {
        foreach (object r in rest)
        {
          if (!Equals(r, obj3))
          {
            return false;
          }
        }
      }
      return h;
    }


#endif


  }
}
