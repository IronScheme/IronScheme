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

#if R6RS
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
  public class Programs : Builtins
  {

    [Builtin]
    public static object Exit()
    {
      return Exit(0);
    }

    [Builtin]
    public static object Exit(object reason)
    {
      if (reason is bool && !(bool)reason)
      {
        Environment.Exit(1);
      }
      int r = RequiresNotNull<int>(reason);
      Environment.Exit(r);
      return Unspecified;
    }
  }
}
#endif
