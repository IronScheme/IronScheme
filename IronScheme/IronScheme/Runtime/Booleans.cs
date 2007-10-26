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



    [Builtin("boolean?")]
    public static bool IsBoolean(object obj)
    {
      return obj is bool; 
    }


    [Builtin("not")]
    public static bool Not(object obj)
    {
      if (obj is bool)
      {
        return !(bool)obj;
      }
      return !IsTrue(obj);
    }


  }
}
