#region License
/* ****************************************************************************
 * Copyright (c) Llewellyn Pritchard. 
 *
 * This source code is subject to terms and conditions of the Microsoft Permissive License. 
 * A copy of the license can be found in the License.html file at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * Microsoft Permissive License.
 *
 * You must not remove this notice, or any other, from this software.
 * ***************************************************************************/
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using IronScheme.Hosting;
using Microsoft.Scripting;
using IronScheme.Runtime;

[assembly: Extension(BuiltinsType=typeof(IronScheme.Runtime.ConsoleBuiltins))]

namespace IronScheme.Runtime
{
  class Program
  {
    static int Main(string[] args)
    {
      return new IronSchemeConsoleHost().Run(args);
    }
  }
}
