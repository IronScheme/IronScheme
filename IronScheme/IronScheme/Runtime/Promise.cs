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

    ICallable prom;

    object result = uninitialized;
    Promise(ICallable prom)
    {
      this.prom = prom;
    }

    public static Promise Make(ICallable c)
    {
      return new Promise(c);
    }

    public object Force()
    {
      return prom.Call();
    }
  }
}
