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
using Microsoft.Scripting;

namespace IronScheme.Runtime
{
  public sealed class Promise
  {
    static readonly object uninitialized = new object();
    Delegate target;
    CodeContext cc;
    object result = uninitialized;
    Promise(CodeContext cc, Delegate target)
    {
      this.cc = cc;
      this.target = target;
    }

    public static Promise Make(CodeContext cc, Delegate target)
    {
      return new Promise(cc, target);
    }

    public object Force()
    {
      if (result == uninitialized)
      {
        if (target is CallTarget0)
        {
          result = ((CallTarget0)target)();
        }
        else
        {
          result = ((CallTargetWithContext0)target)(cc);
        }
      }
      return result;

    }
  }
}
